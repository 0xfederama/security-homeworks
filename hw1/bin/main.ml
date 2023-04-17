open Hw1.Interpreter

let _ = eval (
    Let("pw", CstS "wrong",
        Enclave("encl",
            Secret("pass", CstS "right",
                Secret("check", Fun("s", Prim("=", Var "s", Var "pass")),
                    Gateway("check",
                        End
                    )
                )
            ),
            Let("b", Call(Var "check", Var "pw"), 
                If(Var "b", Print(CstS "password ok"), Print(CstS "password diff"))
            )
        ))
    ) [];;
