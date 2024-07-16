type t

val null : t
(** JavaScript [null] value. *)

val undefined : t
(** JavaScript [undefined] value. *)

val global_this : t
(** JavaScript [globalThis] value. *)

external raw : string -> 'a = "caml_pure_js_expr"

(* {2 Equality} *)

external equal : t -> t -> bool = "caml_js_equals"
external strict_equal : t -> t -> bool = "caml_js_strict_equals"

(** {2 Converters} *)

external of_int : int -> t = "%identity"
external to_int : t -> int = "%identity"
external of_string : string -> t = "caml_jsstring_of_string"
external to_string : t -> string = "caml_string_of_jsstring"
external of_bool : bool -> t = "caml_js_from_bool"
external to_bool : t -> bool = "caml_js_to_bool"
external of_float : float -> t = "caml_js_from_float"
external to_float : t -> float = "caml_js_to_float"
external of_array : 'a array -> t = "caml_js_from_array"
external to_array : t -> 'a array = "caml_js_to_array"
external of_list : t list -> t = "caml_list_to_js_array"
external to_list : t -> t list = "caml_list_of_js_array"

(** {2 Objects and properties} *)

external obj : (string * t) array -> t = "caml_js_object"
external obj_new : t -> t array -> t = "caml_js_new"
external get : t -> t -> t = "caml_js_get"
external set : t -> t -> t -> unit = "caml_js_set"
external del : t -> t -> unit = "caml_js_delete"

(** {2 Function and method helpers} *)

external obj_call : t -> string -> t array -> t = "caml_js_meth_call"
external call : t -> t array -> t = "caml_js_fun_call"
external of_fun : int -> (_ -> _) -> t = "caml_js_wrap_callback_strict"

(** {2 Type helpers} *)

external type_of : t -> string = "caml_js_typeof"
external instance_of : t -> constr:t -> bool = "caml_js_instanceof"

(** {2 Debugger} *)

external debugger : unit -> unit = "debugger"
(** JavaScript [debugger] primitive. *)
