type t

(* Raw *)

val null : t
val undefined : t
val global_this : t
val debugger : unit -> unit

(* Equality and comparision. *)

val equal : t -> t -> bool

(* Converters. *)

val of_string : string -> t
val to_string : t -> string
val of_bool : bool -> t
val to_bool : t -> bool
val of_float : float -> t
val to_float : t -> float
val of_int : int -> t
val to_int : t -> int
val of_js_array : 'a array -> t
val to_js_array : t -> 'a array

(* Objects and properties. *)

val obj : (string * t) array -> t
val obj_new : t -> t array -> t
val obj_get : t -> 'prop -> t
val obj_set : t -> 'prop -> t -> unit
val obj_del : t -> 'prop -> unit

(* Function and method helpers. *)

val obj_call : t -> string -> t array -> t
val fun_call : t -> t array -> t
val of_fun : int -> (_ -> _) -> t

(* Type helpers. *)

val type_of : t -> t
val instance_of : t -> t -> bool
