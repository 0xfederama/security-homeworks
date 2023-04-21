type ide = string
(* type 'v env = (ide * 'v) list *)

module StringMap = Map.Make (String)

(* let rec lookup env x = *)
(*   match env with *)
(*   | [] -> failwith (x ^ " not found in the environment") *)
(*   | (y, v) :: l -> if x = y then v else lookup l x *)

(* let create_env = *)
(*     let env : 'v StringMap.t = StringMap.empty *)
(*     in env *)
let create_env : 'v StringMap.t = StringMap.empty

(* Lookup a key in the environment and return its value, or failwith if not found *)
let lookup env k =
  try StringMap.find k env
  with Failure _ -> failwith (k ^ " not found in the environment")

(* Search for a key in the environment *)
let has_key env k = StringMap.mem k env

(* Insert a new key-value pair into the environment *)
let insert env k v = StringMap.add k v env

(* let rec search env x = *)
(*   match env with *)
(*   | [] -> false *)
(*   | (y, _) :: l -> if x = y then true else search l x *)

let print_int_env env k =
  try
    let v = lookup env k in
    print_endline (string_of_int v);
    v
  with Failure _ -> failwith (k ^ " not found to print")

let print_bool_env env k =
  try
    let v = lookup env k in
    print_endline (string_of_bool v);
    v
  with Failure _ -> failwith (k ^ " not found to print")

let print_str_env env k =
  try
    let v = lookup env k in
    print_endline v;
    v
  with Failure _ -> failwith (k ^ " not found to print")

(* let rec print_int_env env x = *)
(*   match env with *)
(*   | [] -> failwith (x ^ " not found to print") *)
(*   | (y, z) :: l -> *)
(*       if y = x then ( *)
(*         print_endline (string_of_int z); *)
(*         z) *)
(*       else print_int_env l x *)
(***)
(* let rec print_bool_env env x = *)
(*   match env with *)
(*   | [] -> failwith (x ^ " not found to print") *)
(*   | (y, z) :: l -> *)
(*       if y = x then ( *)
(*         print_endline (string_of_bool z); *)
(*         z) *)
(*       else print_bool_env l x *)
(***)
(* let rec print_str_env env x = *)
(*   match env with *)
(*   | [] -> failwith (x ^ " not found to print") *)
(*   | (y, z) :: l -> *)
(*       if y = x then ( *)
(*         print_endline z; *)
(*         z) *)
(*       else print_str_env l x *)
