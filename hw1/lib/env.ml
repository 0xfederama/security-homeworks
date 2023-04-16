type ide = string

type 'v env = (ide * 'v) list

let rec lookup env x = 
    match env with
    | [] -> failwith (x ^ " not found in the environment")
    | (y, v) :: l -> if x = y then v else lookup l x
