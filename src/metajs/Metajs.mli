type js

val global : js
(** See
    {{:https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/globalThis}
    [globalThis]}. *)

val null : js
(** The JavaScript [null] value. *)

val undefined : js
(** The JavaScript [undefined] value. *)

val is_null : js -> bool
(** [is_null js] is [js == null]. *)

val is_undefined : js -> bool
(** [is_undefined js] is [js == undefined]. *)

val is_none : js -> bool
val is_some : js -> bool
val option_of_js : (js -> 'a) -> js -> 'a option
val js_of_option : none:js -> ('a -> js) -> 'a option -> js
val js_equal : js -> js -> bool
val js_of_unit : unit -> js
val unit_of_js : js -> unit
val js_of_string : string -> js
val string_of_js : js -> string
val js_of_bytestring : string -> js
val bytestring_of_js : js -> string
val js_of_bool : bool -> js
val bool_of_js : js -> bool
val js_of_float : float -> js
val float_of_js : js -> float
val js_of_int : int -> js
val int_of_js : js -> int
val new_obj : js -> js array -> js
val obj : (string * js) array -> js
val get : js -> string -> js
val get_path : js -> string list -> js
val set : js -> string -> js -> unit
val set_path : js -> string list -> js -> unit
val delete : js -> string -> unit
val lookup : js -> string -> js option
val lookup_map : (js -> 'a) -> js -> string -> 'a option
val lookup_path : js -> string list -> js option
val lookup_map_path : (js -> 'a) -> js -> string list -> 'a option
val meth_call : js -> string -> js array -> js
val meth_call_unit : js -> string -> js array -> unit
val fun_call : js -> js array -> js
val fun_call_unit : js -> js array -> unit
val callback : arity:int -> (_ -> _) -> js
val typeof : js -> js
val instanceof : js -> js -> bool


val repr : 'a -> js

module Global : sig
  val this : js
  val document : js
  val window : js
  val console : js
end
