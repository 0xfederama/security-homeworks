type ide = string
type 'v env = (ide * 'v) list

let rec lookup env x =
  match env with
  | [] -> failwith (x ^ " not found in the environment")
  | (y, v) :: l -> if x = y then v else lookup l x

let rec search env x =
    match env with
    | [] -> false
    | (y, _) :: l -> if x = y then true else search l x

let rec print_int_env env x =
  match env with
  | [] -> failwith (x ^ " not found to print")
  | (y, z) :: l ->
      if y = x then (
        print_endline (string_of_int z);
        z)
      else print_int_env l x

let rec print_bool_env env x =
  match env with
  | [] -> failwith (x ^ " not found to print")
  | (y, z) :: l ->
      if y = x then (
        print_endline (string_of_bool z);
        z)
      else print_bool_env l x

let rec print_str_env env x =
  match env with
  | [] -> failwith (x ^ " not found to print")
  | (y, z) :: l ->
      if y = x then (
        print_endline z;
        z)
      else print_str_env l x
