type t

val null : t
(** JavaScript [null] value. *)

val undefined : t
(** JavaScript [undefined] value. *)

val global_this : t
(** JavaScript [globalThis] value. *)

(* {2 Equality} *)

val equal : t -> t -> bool
val strict_equal : t -> t -> bool

(** {2 Converters} *)

val of_string : string -> t
val to_string : t -> string
val of_bool : bool -> t
val to_bool : t -> bool
val of_float : float -> t
val to_float : t -> float
val of_int : int -> t
val to_int : t -> int
val of_array : t array -> t
val to_array : t -> t array
val of_list : t list -> t
val to_list : t -> t list

(** {2 Objects and properties} *)

val obj : (string * t) array -> t
val obj_new : t -> t array -> t
val get : t -> t -> t
val set : t -> t -> t -> unit
val del : t -> t -> unit

(** {2 Function and method helpers} *)

val obj_call : t -> string -> t array -> t
val call : t -> t array -> t
val of_fun : int -> (_ -> _) -> t

(** {2 Type helpers} *)

val type_of : t -> string
val instance_of : t -> constr:t -> bool

(** {2 Debugger} *)

val debugger : unit -> unit
(** JavaScript [debugger] primitive. *)
