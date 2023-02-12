include Metajs_external

let is_null v = v == null
let is_undefined v = v == undefined
let is_none v = is_null v || is_undefined v
let is_some v = not (is_none v)
let option_of_js of_js v = if is_none v then None else Some (of_js v)

let js_of_array to_js array =
  Metajs_external.js_of_array (Array.map to_js array)

let array_of_js of_js array_js =
  Array.map of_js (Metajs_external.array_of_js array_js)

let js_of_option ~none to_js = function
  | None -> none
  | Some v -> to_js v

let js_of_unit () = undefined
let unit_of_js _ = ()
let fun_call_unit f args = unit_of_js (fun_call f args)
let meth_call_unit this f args = unit_of_js (meth_call this f args)

let rec obj_get_path obj path =
  match path with
  | [] -> obj
  | k :: path' ->
    let obj' = obj_get obj k in
    obj_get_path obj' path'

let rec obj_set_path obj path value =
  match path with
  | [] -> invalid_arg "set_prop_path: empty path not allowed"
  | [ k ] -> obj_set obj k value
  | k :: path' ->
    let obj' = obj_get obj k in
    obj_set_path obj' path' value

(*
let lookup obj p =
  let v = get obj p in
  if is_none v then None else Some v

let lookup_map f obj path =
  let v = get obj path in
  if is_none v then None else Some (f v)

let rec lookup_path obj path =
  match path with
  | [] -> Some obj
  | k :: path' -> (
    match lookup obj k with
    | None -> None
    | Some obj' -> lookup_path obj' path')

let rec lookup_map_path f obj path =
  match path with
  | [] -> Some (f obj)
  | k :: path' -> (
    match lookup obj k with
    | None -> None
    | Some obj' -> lookup_map_path f obj' path')
*)

external repr : 'a -> js = "%identity"
