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
    1- [ ok ] use a declassified variable
    2- [fail] declassify a variable outside an enclave
    3- [ ok ] use a declassified variable inside untrusted code
    4- [fail] declassify a function
*)
let tests =
  [
    (* 1 - use a declassified variable *)
    execWithoutFailure
      (Enclave
         (Secret ("pass", CstS "pw", Declassify ("pass", CstSkip)), Var "pass"));
    (* 2 - declassify a variable outside an enclave *)
    execWithFailure
      (Enclave
         (Secret ("pass", CstS "pw", CstSkip), Declassify ("pass", CstSkip)));
    (* 3 - use a declassified variable inside untrusted code *)
    execWithoutFailure
      (Enclave
         ( Secret ("pass", CstS "pw", Declassify ("pass", CstSkip)),
           Let ("untr", IncUntrusted (Var "pass"), Execute ("untr", CstSkip)) ));
    (* 4 - declassify a function *)
    execWithFailure
      (Enclave
         ( Secret ("pass", Fun ("arg", CstSkip), Declassify ("pass", CstSkip)),
           Var "pass" ));
  ]

let _ =
  Printf.printf "*** Homework 2 ***\n";
  List.iteri (fun ind funtest -> funtest (ind + 1)) tests
