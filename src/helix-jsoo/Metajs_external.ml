type js = Jsoo_runtime.Js.t

let null = Jsoo_runtime.Js.pure_js_expr "null"
let undefined = Jsoo_runtime.Js.pure_js_expr "undefined"
let js_equal = Jsoo_runtime.Js.equals
let js_of_string = Jsoo_runtime.Js.string
let string_of_js = Jsoo_runtime.Js.to_string
let js_of_bytestring = Jsoo_runtime.Js.bytestring
let bytestring_of_js = Jsoo_runtime.Js.to_bytestring
let js_of_bool = Jsoo_runtime.Js.bool
let bool_of_js = Jsoo_runtime.Js.to_bool
let js_of_float = Jsoo_runtime.Js.number_of_float
let float_of_js = Jsoo_runtime.Js.float_of_number

external js_of_int : int -> js = "%identity"
external int_of_js : js -> int = "%identity"

let obj = Jsoo_runtime.Js.obj
let new_obj = Jsoo_runtime.Js.new_obj

external get : js -> string -> js = "caml_js_get"
external set : js -> string -> js -> unit = "caml_js_set"
external delete : js -> string -> unit = "caml_js_delete"

let fun_call = Jsoo_runtime.Js.fun_call
let meth_call = Jsoo_runtime.Js.meth_call

external callback : arity:int -> (_ -> _) -> js = "caml_js_wrap_callback_strict"

let typeof = Jsoo_runtime.Js.typeof
let instanceof = Jsoo_runtime.Js.instanceof
let global = Jsoo_runtime.Js.pure_js_expr "globalThis"
