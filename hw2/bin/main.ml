open Hw2.Interpreter

let execWithFailure test =
  let value = try run test with Failure _ -> (* print_endline f; *)
                                             Int 1 in
  assert (value = Int 1)

let execWithoutFailure test =
  let value = try run test with Failure _ -> (* print_endline f; *)
                                             Int 0 in
  assert (value <> Int 0)

(* Possible cases (in order with the examples below)
    1- [ ok ] code executes fun gateway
    2- [fail] code accesses secret vars
    3- [fail] enclave declares something not secret/gateway
    4- [ ok ] untrusted code executes fun free
    5- [fail] untrusted code executes fun enclaved
    6- [fail] untrusted code accesses secret var
    7- [fail] execute something not untrusted
    8- [fail] execute something else (var, ...)
    9- [fail] code uses some variable of the untrusted code
   10- [ ok ] execute untrusted code
   11- [fail] enclave declares a variable already declared outside
*)
let tests =
  [
    (* 1 - code executes gateway fun *)
    execWithoutFailure
      (Let
         ( "pw",
           CstS "right",
           Enclave
             ( Secret
                 ( "pass",
                   CstS "right",
                   Secret
                     ( "check",
                       Fun ("arg", Prim ("=", Var "arg", Var "pass")),
                       Gateway ("check", End) ) ),
               Let
                 ( "b",
                   Call (Var "check", Var "pw"),
                   If (Var "b", CstSkip, CstSkip) ) ) ));
    (* 2 - code accesses secret vars *)
    execWithFailure
      (Let
         ( "pw",
           CstS "wrong",
           Enclave
             ( Secret
                 ( "pass",
                   CstS "right",
                   Secret
                     ( "check",
                       Fun ("arg", Prim ("=", Var "arg", Var "pass")),
                       Gateway ("check", End) ) ),
               If (Prim ("=", Var "pw", Var "pass"), CstSkip, CstSkip) ) ));
    (* 3 - enclave declares something not secret/gateway *)
    execWithFailure (Enclave (Let ("fail", CstSkip, CstSkip), End));
    (* 4 - untrusted code executes fun free *)
    execWithoutFailure
      (Let
         ( "f",
           Fun ("arg", CstSkip),
           Let
             ( "untr",
               IncUntrusted (Call (Var "f", CstS "toprint")),
               Execute ("untr", CstSkip) ) ));
    (* 5 - untrusted code executes fun enclaved *)
    execWithFailure
      (Enclave
         ( Secret
             ( "pass",
               CstS "rightpw",
               Secret
                 ( "check",
                   Fun ("s", Prim ("=", Var "s", Var "pass")),
                   Gateway ("check", End) ) ),
           Let
             ( "untr",
               IncUntrusted
                 (Let
                    ( "trypw",
                      CstS "rightpw",
                      Let
                        ( "b",
                          Call (Var "check", Var "trypw"),
                          If (Var "b", CstSkip, CstSkip) ) )),
               Execute ("untr", CstSkip) ) ));
    (* 6 - untrusted code access secret var *)
    execWithFailure
      (Enclave
         ( Secret
             ( "pass",
               CstS "rightpw",
               Secret
                 ( "check",
                   Fun ("s", Prim ("=", Var "s", Var "pass")),
                   Gateway ("check", End) ) ),
           Let
             ( "untr",
               IncUntrusted
                 (Let
                    ( "trypw",
                      CstS "rightpw",
                      Prim ("=", Var "trypw", Var "pass") )),
               Execute ("untr", CstSkip) ) ));
    (* 7 - execute something not untrusted *)
    execWithFailure (Let ("f", Fun ("arg", CstSkip), Execute ("f", CstSkip)));
    (* 8 - execute something else (var, ...) *)
    execWithFailure (Let ("f", CstS "no", Execute ("f", CstSkip)));
    (* 9 - code uses some variable of the untrusted code *)
    execWithFailure
      (Let
         ( "untr",
           IncUntrusted (Let ("x", CstS "untr_str", CstSkip)),
           Execute ("untr", Print (Prim ("=", CstS "s", Var "x"))) ));
    (* 10 - execute untrusted code *)
    execWithoutFailure
      (Let ("untr", IncUntrusted CstSkip, Execute ("untr", CstSkip)));
    (* 11 - enclave declares a variable already declared outside *)
    execWithFailure
      (Let
         ( "outside",
           CstS "str1",
           Enclave (Secret ("outside", CstS "str2", CstSkip), End) ));
  ]

let rec execute_tests t i =
  match t with
  | [] -> print_endline "Tests passed"
  | x :: l ->
      (* print_endline ("Running test case " ^ (string_of_int i)); *)
      x;
      execute_tests l (i + 1)

let _ = execute_tests tests 0
