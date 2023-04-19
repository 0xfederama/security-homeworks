open Hw1.Interpreter

let execWithFailure test env =
  let value = 
    try eval test env
    with Failure _ ->
      (* print_endline f; *)
      Int 1
  in 
  assert (value = Int 1)

let execWithoutFailure test env =
  let value = 
    try eval test env 
    with Failure _ -> 
      (* print_endline f; *)
      Int 0
  in 
  assert (value <> Int 0)

(* Possible cases (in order with the examples below)
 1- [ ok ] code executes fun gateway
 2- [fail] code accesses secret vars
 3- [fail] enclave declares something not secret/gateway
 4- [ ok ] include executes fun free
 5- [fail] include executes fun enclaved
 6- [fail] include access secret var
 7- [fail] execute something not untrusted
 8- [fail] execute something else (var, ...)
*)
let tests = 
  [
    (* 1 - code executes gateway fun *)
    execWithoutFailure (
        Let("pw", CstS "right",
            Enclave("encl",
                Secret("pass", CstS "right",
                    Secret("check", Fun("arg", Prim("=", Var "arg", Var "pass")),
                        Gateway("check",
                            End
                        )
                    )
                ),
                Let("b", Call(Var "check", Var "pw"), 
                    If(Var "b", CstSkip, CstSkip)
                )
            )
        )
    ) [];
    (* 2 - code accesses secret vars *)
    execWithFailure (
        Let("pw", CstS "wrong",
            Enclave("encl",
                Secret("pass", CstS "right",
                    Secret("check", Fun("arg", Prim("=", Var "arg", Var "pass")),
                        Gateway("check",
                            End
                        )
                    )
                ),
                If(Prim("=", Var "pw", Var "pass"), CstSkip, CstSkip)
            )
        )
    ) [];
    (* 3 - enclave declares something not secret/gateway *)
    execWithFailure (
        Enclave("encl",
            Let("fail", CstSkip, CstSkip),
            End
        )
    ) [];
    (* 4 - include executes fun free *)
    execWithoutFailure (
        Let("f", Fun("arg", CstSkip),
            Let("untr",
                IncUntrusted(
                    Call(Var "f", CstS "toprint")
                ),
                Execute("untr", CstSkip)
            )
        )
    ) [];
    (* 5 - include executes fun enclaved *)
    execWithFailure (
        Enclave("enc",
            Secret("pass", CstS "rightpw",
                Secret("check", Fun("s", Prim("=", Var "s", Var "pass")),
                    Gateway("check",
                        End
                    )
                )
            ),
            Let("untr",
                IncUntrusted(
                    Let("trypw", CstS "rightpw",
                        Let("b", Call(Var "check", Var "trypw"),
                            If(Var "b", CstSkip, CstSkip)
                        )
                    )
                ),
                Execute("untr", CstSkip)
            )
        )
    ) [];
    (* 6 - include access secret var *)
    execWithFailure (
        Enclave("enc",
            Secret("pass", CstS "rightpw",
                Secret("check", Fun("s", Prim("=", Var "s", Var "pass")),
                    Gateway("check",
                        End
                    )
                )
            ),
            Let("untr",
                IncUntrusted(
                    Let("trypw", CstS "rightpw",
                        Prim("=", Var "trypw", Var "pass")
                    )
                ),
                Execute("untr", CstSkip)
            )
        )
    ) [];
    (* 7 - execute something not untrusted *)
    execWithFailure(
        Let("f", Fun("arg", CstSkip),
            Execute("f", CstSkip)
        )
    ) [];
    (* 8 - execute something else (var, ...) *)
    execWithFailure (
        Let("f", CstS "no",
            Execute("f", CstSkip)
        )

    ) [];
  ]

let rec execute_tests t i =
  match t with
  | [] -> print_endline "Tests passed"
  | x :: l ->
      (* print_endline ("Running test case " ^ (string_of_int i)); *)
      x;
      execute_tests l (i+1)

let _ = execute_tests tests 0
