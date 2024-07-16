type t

external raw : string -> 'a = "caml_pure_js_expr"

let global_this = raw "globalThis"
let null = raw "null"
let undefined = raw "undefined"

external debugger : unit -> unit = "debugger"
external equal : t -> t -> bool = "caml_js_equals"
external strict_equal : t -> t -> bool = "caml_js_strict_equals"

(* Primitives *)
external of_int : int -> t = "%identity"
external to_int : t -> int = "%identity"
external of_string : string -> t = "caml_jsstring_of_string"
external to_string : t -> string = "caml_string_of_jsstring"
external of_bool : bool -> t = "caml_js_from_bool"
external to_bool : t -> bool = "caml_js_to_bool"
external of_float : float -> t = "caml_js_from_float"
external to_float : t -> float = "caml_js_to_float"

(* Array *)
external of_array : 'a array -> t = "caml_js_from_array"
external to_array : t -> 'a array = "caml_js_to_array"

(* List *)
external of_list : t list -> t = "caml_list_to_js_array"
external to_list : t -> t list = "caml_list_of_js_array"

(* Obj *)
external get : t -> t -> t = "caml_js_get"
external set : t -> t -> t -> unit = "caml_js_set"
external del : t -> t -> unit = "caml_js_delete"
external obj : (string * t) array -> t = "caml_js_object"
external obj_new : t -> t array -> t = "caml_js_new"
external obj_call : t -> string -> t array -> t = "caml_js_meth_call"

(* Fun *)
external call : t -> t array -> t = "caml_js_fun_call"
external of_fun : int -> (_ -> _) -> t = "caml_js_wrap_callback_strict"

(* type_of *)

external type_of : t -> string = "caml_js_typeof"

(* instance_of *)

external instance_of : t -> constr:t -> bool = "caml_js_instanceof"
