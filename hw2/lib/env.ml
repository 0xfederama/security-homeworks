type ide = string

(* Lookup a key in the environment and return its value, or failwith if not found *)
let lookup env k =
  try Hashtbl.find env k
  with Not_found -> failwith (k ^ " not found in the environment")

let has_key env k = Hashtbl.mem env k

let insert env k v =
  Hashtbl.add env k v;
  env

let replace env k v =
  Hashtbl.replace env k v;
  env

let clone env = Hashtbl.copy env

let print_int_env env k =
  try
    let v = lookup env k in
    let f = fst v in
    print_endline (string_of_int f);
    v
  with Failure _ -> failwith (k ^ " not found to print")

let print_bool_env env k =
  try
    let v = lookup env k in
    let f = fst v in
    print_endline (string_of_bool f);
    v
  with Failure _ -> failwith (k ^ " not found to print")

let print_str_env env k =
  try
    let v = lookup env k in
    let f = fst v in
    print_endline f;
    v
  with Failure _ -> failwith (k ^ " not found to print")
