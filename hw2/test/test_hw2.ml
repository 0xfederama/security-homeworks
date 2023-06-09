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
    1- [] 
*)
let tests =
  [
    (* TODO: remove tests hw1 *)
    execWithFailure (Let ("f", Fun ("arg", CstSkip), Execute ("f", CstSkip)));
    execWithoutFailure
      (Let ("untr", IncUntrusted CstSkip, Execute ("untr", CstSkip)));
  ]

let _ =
  Printf.printf "*** Homework 2 ***\n";
  List.iteri (fun ind funtest -> funtest (ind + 1)) tests
