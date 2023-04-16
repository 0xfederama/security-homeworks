open Env

type expr =
  | CstI of int
  | CstB of bool
  | Var of ide
  | Prim of ide * expr * expr
  | If of expr * expr * expr
  | Let of ide * expr * expr
  | Fun of ide * expr
  | Call of expr * expr

type value = Int of int | Bool of bool | Closure of ide * expr * value env

let rec eval (e: expr) (env: value env) : value =
  match e with
  | CstI i -> Int i
  | CstB b -> Bool b
  | Var x -> lookup env x
  | Prim (op, e1, e2) -> (
      let v1 = eval e1 env in
      let v2 = eval e2 env in
      match (op, v1, v2) with
      | "+", Int i1, Int i2 -> Int (i1 + i2)
      | "-", Int i1, Int i2 -> Int (i1 - i2)
      | "*", Int i1, Int i2 -> Int (i1 * i2)
      | "=", Int i1, Int i2 -> Bool (if i1 = i2 then true else false)
      | ">", Int i1, Int i2 -> Bool (if i1 > i2 then true else false)
      | "<", Int i1, Int i2 -> Bool (if i1 < i2 then true else false)
      | "<=", Int i1, Int i2 -> Bool (if i1 <= i2 then true else false)
      | ">=", Int i1, Int i2 -> Bool (if i1 >= i2 then true else false)
      | "!=", Int i1, Int i2 -> Bool (if i1 <> i2 then true else false)
      | _ -> failwith "unknown primitive or wrong type")
  | If (e1, e2, e3) -> (
      match eval e1 env with
      | Bool true -> eval e2 env
      | Bool false -> eval e3 env
      | _ -> failwith "not a boolean guard")
  | Let (x, er, body) ->
      let xval = eval er env in
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
  (* | _ -> failwith "eval error" *)
