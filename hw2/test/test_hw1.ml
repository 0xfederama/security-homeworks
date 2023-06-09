open Hw2.Interpreter

let execWithFailure test ind =
  try
    run test |> ignore;
    Printf.printf "\027[1;31m[FAIL]\027[0m Test #%d didn't fail but should\n"
      ind
  with Failure f -> Printf.printf "[ OK ] Test #%d passed: %s\n" ind f

let execWithoutFailure test ind =
  try
    run test |> ignore;
    Printf.printf "[ OK ] Test #%d passed\n" ind
  with Failure f ->
    Printf.printf "\027[1;31m[FAIL]\027[0m Test #%d failed: %s\n" ind f

(* Possible cases (in order with the examples below)
    1- [fail] code prints secret var
    2- [ ok ] code executes fun gateway
    3- [fail] code accesses secret vars
    4- [fail] enclave declares something not secret/gateway
    5- [ ok ] untrusted code executes fun free
    6- [fail] untrusted code executes fun enclaved
    7- [fail] untrusted code accesses secret var
    8- [fail] execute something not untrusted
    9- [fail] execute something else (var, ...)
   10- [fail] code uses some variable of the untrusted code
   11- [ ok ] execute untrusted code
   12- [fail] enclave declares a variable already declared outside
   13- [ ok ] call a free enclaved function from the outside
   14- [fail] gateway something that is not a function
   15- [fail] define an enclave inside untrusted code
*)
let tests =
  [
    (* 1- code prints secret variable *)
    execWithFailure
      (Enclave (Secret ("pass", CstS "pw", CstSkip), Print (Var "pass")));
    (* 2 - code executes gateway fun *)
    execWithoutFailure
      (Let
         ( "pw",
           CstS "right",
           Enclave
             ( Secret
                 ( "pass",
                   CstS "right",
                   Secret
                     ( "checkpw",
                       Fun ("arg", Prim ("=", Var "arg", Var "pass")),
                       Gateway ("checkpw", CstSkip) ) ),
               Let
                 ( "b",
                   Call (Var "checkpw", Var "pw"),
                   If (Var "b", CstSkip, CstSkip) ) ) ));
    (* 3 - code accesses secret vars *)
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
                       Gateway ("check", CstSkip) ) ),
               If (Prim ("=", Var "pw", Var "pass"), CstSkip, CstSkip) ) ));
    (* 4 - enclave declares public var *)
    execWithoutFailure (Enclave (Let ("free", CstSkip, CstSkip), CstSkip));
    (* 5 - untrusted code executes fun free *)
    execWithoutFailure
      (Let
         ( "f",
           Fun ("arg", CstSkip),
           Let
             ( "untr",
               IncUntrusted (Call (Var "f", CstS "toprint")),
               Execute ("untr", CstSkip) ) ));
    (* 6 - untrusted code executes fun gateway *)
    execWithFailure
      (Enclave
         ( Secret
             ( "pass",
               CstS "rightpw",
               Secret
                 ( "check",
                   Fun ("s", Prim ("=", Var "s", Var "pass")),
                   Gateway ("check", CstSkip) ) ),
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
    (* 7 - untrusted code access secret var *)
    execWithFailure
      (Enclave
         ( Secret
             ( "pass",
               CstS "rightpw",
               Secret
                 ( "check",
                   Fun ("s", Prim ("=", Var "s", Var "pass")),
                   Gateway ("check", CstSkip) ) ),
           Let
             ( "untr",
               IncUntrusted
                 (Let
                    ( "trypw",
                      CstS "rightpw",
                      Prim ("=", Var "trypw", Var "pass") )),
               Execute ("untr", CstSkip) ) ));
    (* 8 - execute something not untrusted *)
    execWithFailure (Let ("f", Fun ("arg", CstSkip), Execute ("f", CstSkip)));
    (* 9 - execute something else (var, ...) *)
    execWithFailure (Let ("f", CstS "no", Execute ("f", CstSkip)));
    (* 10 - code uses some variable of the untrusted code *)
    execWithFailure
      (Let
         ( "untr",
           IncUntrusted (Let ("x", CstS "untr_str", CstSkip)),
           Execute ("untr", Prim ("=", CstS "s", Var "x")) ));
    (* 11 - execute untrusted code *)
    execWithoutFailure
      (Let ("untr", IncUntrusted CstSkip, Execute ("untr", CstSkip)));
    (* 12 - enclave declares a variable already declared outside *)
    execWithFailure
      (Let
         ( "outside",
           CstS "str1",
           Enclave (Secret ("outside", CstS "str2", CstSkip), CstSkip) ));
    (* 13 - call a free enclaved function from the outside *)
    execWithoutFailure
      (Enclave
         (Let ("fun", Fun ("arg", CstSkip), CstSkip), Call (Var "fun", CstSkip)));
    (* 14 - gateway something that is not a function *)
    execWithFailure
      (Enclave
         ( Secret ("pass", CstS "pw", Gateway ("pass", CstSkip)),
           Print (Var "pass") ));
    (* 15 - define an enclave inside untrusted code *)
    execWithFailure
      (Let
         ( "untr",
           IncUntrusted (Enclave (CstSkip, CstSkip)),
           Execute ("untr", CstSkip) ));
  ]

let _ =
  Printf.printf "*** Homework 1 ***\n";
  List.iteri (fun ind funtest -> funtest (ind + 1)) tests
