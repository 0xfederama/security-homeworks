open Env

type expr =
  | CstSkip (* mainly for testing purposes *)
  | CstI of int
  | CstB of bool
  | CstS of string
  | Var of ide
  | Prim of ide * expr * expr
  | If of expr * expr * expr
  | Let of ide * expr * expr
  | Fun of ide * expr
  | Call of expr * expr
  | Enclave of expr * expr
  | Secret of ide * expr * expr
  | Gateway of ide * expr
  | Print of expr
  | IncUntrusted of expr
  | Execute of ide * expr

type sec_level =
  | High (* inside enclave *)
  | Normal (* normal code *)
  | Low (* untrusted code *)

type value =
  | Skip
  | Int of int
  | Bool of bool
  | String of string
  | Closure of ide * expr * (ide, value * sec_level) Hashtbl.t
  | UntrustedClosure of expr

let eval_prim g e1 e2 =
  match (g, e1, e2) with
  | "+", Int i1, Int i2 -> Int (i1 + i2)
  | "-", Int i1, Int i2 -> Int (i1 - i2)
  | "*", Int i1, Int i2 -> Int (i1 * i2)
  | "=", Int i1, Int i2 -> Bool (i1 = i2)
  | ">", Int i1, Int i2 -> Bool (i1 > i2)
  | "<", Int i1, Int i2 -> Bool (i1 < i2)
  | "<=", Int i1, Int i2 -> Bool (i1 <= i2)
  | ">=", Int i1, Int i2 -> Bool (i1 >= i2)
  | "!=", Int i1, Int i2 -> Bool (i1 <> i2)
  | "=", String s1, String s2 -> Bool (s1 = s2)
  | "!=", String s1, String s2 -> Bool (s1 <> s2)
  | _ -> failwith "unknown primitive or wrong types"

let eval_print x =
  match x with
  | Int i ->
      print_endline (string_of_int i);
      Int i
  | Bool b ->
      print_endline (string_of_bool b);
      Bool b
  | String s ->
      print_endline s;
      String s
  | _ -> failwith "can only print int, bool or string"

let check_security has wants =
  (* can only execute same level or lower *)
  let can =
    match has with
    | Low -> wants = Low
    | Normal -> wants = Low || wants = Normal
    | High -> true
  in
  if can then has else failwith "cannot access var at higher security level"

let rec eval (e : expr) (env : (ide, value * sec_level) Hashtbl.t)
    (trust : sec_level) : value * sec_level =
  match e with
  | CstSkip -> (Skip, trust)
  | CstI i -> (Int i, trust)
  | CstB b -> (Bool b, trust)
  | CstS s -> (String s, trust)
  | Var x ->
      let var = lookup env x in
      let varval = fst var in
      let varsec = snd var in
      (varval, check_security trust varsec)
  | Print x ->
      let xr = eval x env trust in
      let xval = fst xr in
      (eval_print xval, trust)
  | Prim (op, e1, e2) ->
      let v1 = fst (eval e1 env trust) in
      let v2 = fst (eval e2 env trust) in
      (eval_prim op v1 v2, trust)
  | If (e1, e2, e3) -> (
      match eval e1 env trust with
      | Bool true, _ -> eval e2 env trust
      | Bool false, _ -> eval e3 env trust
      | _ -> failwith "not a boolean guard")
  | Let (x, body, rest) ->
      if has_key env x then failwith (x ^ " already defined")
      else
        let xval = fst (eval body env trust) in
        let xnew = (xval, Low) in
        insert env x xnew |> ignore;
        eval rest env trust
  | Fun (x, body) -> (Closure (x, body, clone env), trust)
  | Call (funname, arg) -> (
      let funclosure = eval funname env trust in
      let funval = fst funclosure in
      match funval with
      | Closure (x, body, decenv) ->
          let xval = eval arg env High in
          let bodyenv = clone (insert decenv x xval) in
          eval body bodyenv High
      | _ -> failwith "call not on a function")
  | Enclave (body, rest) ->
      if trust = Low then failwith "cannot define enclave inside untrusted code"
      else (
        eval body env High |> ignore;
        eval rest env Normal)
  | Secret (id, body, rest) ->
      if trust <> High then
        failwith "secrets can be only defined inside enclaves"
      else if has_key env id then failwith (id ^ " already defined")
      else
        let idval = eval body env High in
        let idvalenc =
          match fst idval with
          | Closure (a1, b1, c1) -> (Closure (a1, b1, clone c1), High)
          | _ -> idval
        in
        insert env id (fst idvalenc, High) |> ignore;
        eval rest env High
  | Gateway (id, rest) ->
      if trust <> High then
        failwith "gateways can be only defined inside enclaves"
      else if not (has_key env id) then failwith (id ^ " not defined")
      else
        let idval = lookup env id in
        let newval =
          match idval with
          | x, High -> (x, Normal)
          | _, _ -> failwith "can only gateway a secret"
        in
        replace env id newval |> ignore;
        eval rest env High
  | IncUntrusted body ->
      if trust = High then failwith "cannot include untrusted code in enclaves"
      else (UntrustedClosure body, Low)
  | Execute (id, rest) ->
      if trust = High then failwith "cannot execute untrusted code in enclaves"
      else
        let x = lookup env id in
        let xval = fst x in
        (match xval with
        | UntrustedClosure body -> eval body (clone env) Low
        | _ -> failwith (id ^ " is not untrusted code"))
        |> ignore;
        eval rest env Normal

let run code =
  let henv : (string, value * sec_level) Hashtbl.t = Hashtbl.create 128 in
  eval code henv Normal |> ignore
