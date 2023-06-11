type t

external _pure_js_expr : string -> 'a = "caml_pure_js_expr"

let global_this = _pure_js_expr "globalThis"
let null = _pure_js_expr "null"
let undefined = _pure_js_expr "undefined"

external debugger : unit -> unit = "debugger"
external equal : t -> t -> bool = "caml_js_equals"

(* Primitives *)
external of_string : string -> t = "caml_jsstring_of_string"
external to_string : t -> string = "caml_string_of_jsstring"
external of_bool : bool -> t = "caml_js_from_bool"
external to_bool : t -> bool = "caml_js_to_bool"
external of_float : float -> t = "caml_js_from_float"
external to_float : t -> float = "caml_js_to_float"

(* Array *)
external of_js_array : 'a array -> t = "caml_js_from_array"
external to_js_array : t -> 'a array = "caml_js_to_array"
external of_int : int -> t = "%identity"
external to_int : t -> int = "%identity"

(* Obj *)
external obj : (string * t) array -> t = "caml_js_object"
external obj_new : t -> t array -> t = "caml_js_new"
external obj_get : t -> 'prop -> t = "caml_js_get"
external obj_set : t -> 'prop -> t -> unit = "caml_js_set"
external obj_del : t -> 'prop -> unit = "caml_js_delete"
external obj_call : t -> string -> t array -> t = "caml_js_meth_call"

(* Fun *)
external fun_call : t -> t array -> t = "caml_js_fun_call"
external of_fun : int -> (_ -> _) -> t = "caml_js_wrap_callback_strict"

(* Type *)
external type_of : t -> t = "caml_js_typeof"
external instance_of : t -> t -> bool = "caml_js_instanceof"
