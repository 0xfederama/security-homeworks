type ide = string

(* Lookup a key in the environment and return its value, or failwith if not found *)
let lookup env k =
  try Hashtbl.find env k
  with Not_found -> failwith (k ^ " not found in the environment")

(* Search for a key in the environment *)
let has_key env k = Hashtbl.mem env k

(* Insert a new key-value pair into the environment *)
let insert env k v = 
  Hashtbl.add env k v;
  env

let replace env k v =
  Hashtbl.replace env k v;
  env

let clone env = Hashtbl.copy env

(* let print_v_env env k =
    try
      let v = lookup env k in
      let f = fst(v) in (
      match f with
        | Int(val) ->
            let _ = print_endline (string_of_int val) in
            val
        | Bool(val) ->
            let _ = print_endline (string_of_bool val) in
            val
        | String(val) ->
            let _ = print_endline val in
            val
        | _ -> failwith ("can only print int, bool or string"))
    with Failure _ -> failwith (k ^ " not found to print") *)

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
