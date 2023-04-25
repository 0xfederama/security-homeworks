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
  | End
  | IncUntrusted of expr
  | Execute of ide * expr

type security = Enclaved | Free | Untrusted

type value =
  | Skip
  | Int of int
  | Bool of bool
  | String of string
  | Closure of ide * expr * value StringMap.t * security
  | EnclaveClosure
  | UntrustedClosure of expr

let eval_prim g e1 e2 =
  match (g, e1, e2) with
  | "+", Int i1, Int i2 -> Int (i1 + i2)
  | "-", Int i1, Int i2 -> Int (i1 - i2)
  | "*", Int i1, Int i2 -> Int (i1 * i2)
  | "=", Int i1, Int i2 -> Bool (if i1 = i2 then true else false)
  | ">", Int i1, Int i2 -> Bool (if i1 > i2 then true else false)
  | "<", Int i1, Int i2 -> Bool (if i1 < i2 then true else false)
  | "<=", Int i1, Int i2 -> Bool (if i1 <= i2 then true else false)
  | ">=", Int i1, Int i2 -> Bool (if i1 >= i2 then true else false)
  | "!=", Int i1, Int i2 -> Bool (if i1 <> i2 then true else false)
  | "=", String s1, String s2 -> Bool (if s1 = s2 then true else false)
  | "!=", String s1, String s2 -> Bool (if s1 <> s2 then true else false)
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

let rec eval (e : expr) (env : 'v StringMap.t) : value =
  match e with
  | CstSkip -> Skip
  | CstI i -> Int i
  | CstB b -> Bool b
  | CstS s -> String s
  | Var x -> lookup env x
  | Print x ->
      let xval = eval x env in
      eval_print xval
  | Prim (op, e1, e2) ->
      let v1 = eval e1 env in
      let v2 = eval e2 env in
      eval_prim op v1 v2
  | If (e1, e2, e3) -> (
      match eval e1 env with
      | Bool true -> eval e2 env
      | Bool false -> eval e3 env
      | _ -> failwith "not a boolean guard")
  | Let (x, rhs, body) ->
      if has_key env x then failwith (x ^ " already defined")
      else
        let xval = eval rhs env in
        let letenv = insert env x xval in
        eval body letenv
  | Fun (x, body) -> Closure (x, body, env, Free)
  | Call (eFun, eArg) -> (
      let fClosure = eval eFun env in
      match fClosure with
      | Closure (x, body, decenv, sec) ->
          if sec = Untrusted then
            failwith "untrusted code must be executed inside \"execute\""
          else
            let xval = eval eArg env in
            let bodyenv = insert decenv x xval in
            eval body bodyenv
      | _ -> failwith "call not on a function")
  | Enclave (rhs, body) ->
      let rec eval_enc r glob_env enc_env =
        match r with
        | Secret (id, rhs, b) ->
            if has_key env id then failwith (id ^ " already defined")
            else
              let idval = eval rhs enc_env in
              let idvalenc =
                match idval with
                | Closure (a1, b1, c1, _) -> Closure (a1, b1, c1, Enclaved)
                | _ -> idval
              in
              let enc_env = insert enc_env id idvalenc in
              eval_enc b glob_env enc_env
        | Gateway (id, b) ->
            if has_key env id then failwith (id ^ " already defined")
            else
              let idval = lookup enc_env id in
              let glob_env = insert env id idval in
              eval_enc b glob_env enc_env
        | CstSkip -> Skip
        | End ->
            eval body glob_env
        | _ -> failwith "not a fun, secret, gateway or end"
      in
      eval_enc rhs env env
  | Secret (_, _, _) -> failwith "secrets can only be defined inside enclaves"
  | Gateway (_, _) -> failwith "gateways can be only defined inside enclaves"
  | End -> failwith "end can only end an enclave or an include"
  | IncUntrusted rhs -> UntrustedClosure rhs
  | Execute (x, body) ->
      let xval = lookup env x in
      let _ =
        match xval with
        | UntrustedClosure rhs ->
            let rec eval_untr r uenv =
              match r with
              | CstSkip -> Skip
              | CstI i -> Int i
              | CstB b -> Bool b
              | CstS s -> String s
              | Var x -> lookup uenv x
              | Print x ->
                  let xval = eval_untr x uenv in
                  eval_print xval
              | Prim (op, e1, e2) ->
                  let v1 = eval_untr e1 uenv in
                  let v2 = eval_untr e2 uenv in
                  eval_prim op v1 v2
              | If (e1, e2, e3) -> (
                  match eval_untr e1 uenv with
                  | Bool true -> eval_untr e2 uenv
                  | Bool false -> eval_untr e3 uenv
                  | _ -> failwith "not a boolean guard")
              | Fun (y, b) -> Closure (y, b, env, Untrusted)
              | Call (eFun, eArg) -> (
                  let fClosure = eval_untr eFun uenv in
                  match fClosure with
                  | Closure (x, body, decenv, sec) ->
                      if sec = Enclaved then
                        failwith
                          "can't call enclaved functions in untrusted code"
                      else
                        let xval = eval_untr eArg uenv in
                        let bodyenv = insert decenv x xval in
                        eval_untr body bodyenv
                  | _ -> failwith "call not on a function")
              | Let (y, rh, bod) ->
                  if has_key uenv y then failwith (y ^ " already defined")
                  else
                    let yval = eval_untr rh uenv in
                    let uenv = insert uenv y yval in
                    eval_untr bod uenv
              | Enclave _ -> failwith "can't use enclave inside include"
              | End -> failwith "should not need end"
              | Secret (_, _, _) -> failwith "can't use secret inside untrusted"
              | Gateway (_, _) -> failwith "can't use gateway inside untrusted"
              | IncUntrusted _ ->
                  failwith "can't include untrusted code inside untrusted"
              | Execute _ ->
                  failwith "can't execute untrusted code inside untrusted"
            in
            eval_untr rhs env
        | _ -> failwith (x ^ " is not untrusted code")
      in
      eval body env

let run code =
  let env = create_env in
  eval code env
