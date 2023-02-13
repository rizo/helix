type t = Jsoo_runtime.Js.t

let global = Jsoo_runtime.Js.pure_js_expr "globalThis"
let null = Jsoo_runtime.Js.pure_js_expr "null"
let undefined = Jsoo_runtime.Js.pure_js_expr "undefined"
let equal = Jsoo_runtime.Js.equals
let of_string = Jsoo_runtime.Js.string
let to_string = Jsoo_runtime.Js.to_string
let of_bool = Jsoo_runtime.Js.bool
let to_bool = Jsoo_runtime.Js.to_bool
let of_float = Jsoo_runtime.Js.number_of_float
let to_float = Jsoo_runtime.Js.float_of_number

(* Array *)
let of_js_array = Jsoo_runtime.Js.array
let to_js_array = Jsoo_runtime.Js.to_array

external of_int : int -> t = "%identity"
external to_int : t -> int = "%identity"

let obj = Jsoo_runtime.Js.obj
let obj_new = Jsoo_runtime.Js.new_obj

external obj_get : t -> string -> t = "caml_js_get"
external obj_set : t -> string -> t -> unit = "caml_js_set"
external obj_del : t -> string -> unit = "caml_js_delete"

let fun_call = Jsoo_runtime.Js.fun_call
let obj_call = Jsoo_runtime.Js.meth_call

external of_fun : int -> (_ -> _) -> t = "caml_js_wrap_callback_strict"

let typeof = Jsoo_runtime.Js.typeof
let instanceof = Jsoo_runtime.Js.instanceof
