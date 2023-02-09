type js

(* Standard values. *)

val global : js
val null : js
val undefined : js

(* Equality and comparision. *)

val js_equal : js -> js -> bool

(* Converters. *)

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

(* Objects and properties. *)

val obj : (string * js) array -> js

val new_obj : js -> js array -> js

val get : js -> string -> js
val set : js -> string -> js -> unit
val delete : js -> string -> unit

(* Function and method helpers. *)

val meth_call : js -> string -> js array -> js
val fun_call : js -> js array -> js
val callback : arity:int -> (_ -> _) -> js

(* Type helpers. *)

val typeof : js -> js
val instanceof : js -> js -> bool