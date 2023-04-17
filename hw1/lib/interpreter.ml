open Env

type expr =
  | CstI of int
  | CstB of bool
  | CstS of string
  | Var of ide
  | Prim of ide * expr * expr
  | If of expr * expr * expr
  | Let of ide * expr * expr
  | Fun of ide * expr
  | Call of expr * expr
  | Enclave of ide * expr * expr
  | Secret of ide * expr * expr
  | Gateway of ide * expr
  | Print of expr
  | End

type value =
  | Int of int
  | Bool of bool
  | String of string
  | Closure of ide * expr * value env

let rec eval (e : expr) (env : value env) : value =
  match e with
  | CstI i -> Int i
  | CstB b -> Bool b
  | CstS s -> String s
  | Var x -> lookup env x
  | Print x -> (
      let xval = eval x env in
      match xval with
      | Int i -> print_endline(string_of_int i); Int i
      | Bool b -> print_endline(string_of_bool b); Bool b
      | String s -> print_endline(s); String s
      | _ -> failwith "can only print int, bool or string"
  )
  | Prim (op, e1, e2) -> (
      let v1 = eval e1 env in
      let v2 = eval e2 env in
      match (op, v1, v2) with
      | "+", Int i1, Int i2 -> Int (i1 + i2)
      | "-", Int i1, Int i2 -> Int (i1 - i2)
      | "*", Int i1, Int i2 -> Int (i1 * i2)
      | "=", Int i1, Int i2 -> Bool (if i1 = i2 then true else false)
      | "=", String s1, String s2 -> Bool (if s1 = s2 then true else false)
      | ">", Int i1, Int i2 -> Bool (if i1 > i2 then true else false)
      | "<", Int i1, Int i2 -> Bool (if i1 < i2 then true else false)
      | "<=", Int i1, Int i2 -> Bool (if i1 <= i2 then true else false)
      | ">=", Int i1, Int i2 -> Bool (if i1 >= i2 then true else false)
      | "!=", Int i1, Int i2 -> Bool (if i1 <> i2 then true else false)
      | "!=", String s1, String s2 -> Bool (if s1 <> s2 then true else false)
      | _ -> failwith "unknown primitive or wrong type")
  | If (e1, e2, e3) -> (
      match eval e1 env with
      | Bool true -> eval e2 env
      | Bool false -> eval e3 env
      | _ -> failwith "not a boolean guard")
  | Let (x, rhs, body) ->
      (* if Some (lookup env x) <> None then *)
      (*   failwith (x ^ " already in environment") *)
      (* else *)
        let xval = eval rhs env in
        let letenv = (x, xval) :: env in
        eval body letenv
  | Fun (x, body) -> Closure (x, body, env)
  | Call (eFun, eArg) -> (
      let fClosure = eval eFun env in
      match fClosure with
      | Closure (x, body, decenv) ->
          let xval = eval eArg env in
          let bodyenv = (x, xval) :: decenv in
          eval body bodyenv
      | _ -> failwith "call not on a function")
  | Enclave (_, rhs, body) ->
      (* if Some (lookup env x) <> None then *)
      (*   failwith (x ^ " already in environment") *)
      (* else *)
        let rec eval_enc r glob_env enc_env =
          match r with
          | Fun (x, body) -> Closure (x, body, enc_env)
          | Secret (id, rhs, b) ->
              (* if Some (lookup enc_env id) <> None then *)
              (*   failwith (id ^ " already in the enviroment") *)
              (* else *)
                let idval = eval rhs enc_env in
                let enc_env = (id, idval) :: enc_env in
                eval_enc b glob_env enc_env
          | Gateway (id, b) ->
              let idval = lookup enc_env id in
              let glob_env = (id, idval) :: env in
              eval_enc b glob_env enc_env
          | End -> eval body glob_env
          | _ -> failwith "not a fun, secret or gateway"
        in
        eval_enc rhs env env
  | Secret (_, _, _) -> failwith "secrets can only be defined inside enclaves"
  | Gateway (_, _) -> failwith "gateways can be only defined inside enclaves"
  | End -> failwith "end can only end an enclave"
(* | _ -> failwith "eval error" *)
