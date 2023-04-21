type ide = string

module StringMap = Map.Make (String)

let create_env : 'v StringMap.t = StringMap.empty

(* Lookup a key in the environment and return its value, or failwith if not found *)
let lookup env k =
  try StringMap.find k env
  with Not_found -> failwith (k ^ " not found in the environment")

(* Search for a key in the environment *)
let has_key env k = StringMap.mem k env

(* Insert a new key-value pair into the environment *)
let insert env k v = StringMap.add k v env

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
