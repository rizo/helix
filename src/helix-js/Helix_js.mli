type t
(** JavaScript values. *)

type js = t
(** Alias for {!type:t}. *)

val null : js
(** The JavaScript [null] value. *)

val undefined : js
(** The JavaScript [undefined] value. *)

exception Undefined_property of string
(** An exception raised when an unexpected {!val:undefined} property is encountered. *)

val debugger : unit -> unit
val log : 'a -> unit
val log2 : 'a -> 'b -> unit
val log3 : 'a -> 'b -> 'c -> unit

val is_null : js -> bool
(** [is_null t] is [t == null]. *)

val is_undefined : js -> bool
(** [is_undefined t] is [t == undefined]. *)

(** {2 Type helpers} *)

val type_of : js -> string
val instance_of : js -> constructor:js -> bool

(** {2 Equality} *)

val equal : js -> js -> bool

(** {2 Global values} *)

val global : string -> js
(** [global name] is [globalThis\[name\]]. If this evaluates to {!val:undefined},
    {!exception:Undefined_property} is raised. *)

val global_this : js
(** See
    {{:https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/globalThis}
    [globalThis]}. *)

(** {2 Value encoding} *)

type 'a encoder = 'a -> js

val encode : 'a encoder -> 'a -> js

module Encoder : sig
  val unit : unit encoder
  val int : int encoder
  val float : float encoder
  val js : js encoder
  val bool : bool encoder
  val string : string encoder
  val array : 'a encoder -> 'a array encoder
  val array_js : js array encoder
  val pair : 'a encoder -> 'b encoder -> ('a * 'b) encoder
  val triple : 'a encoder -> 'b encoder -> 'c encoder -> ('a * 'b * 'c) encoder
  val nullable : 'a encoder -> 'a option encoder
  val optional : 'a encoder -> 'a option encoder
  val obj : (string * t) list -> t
  val fun0 : (unit -> 'r) encoder
  val fun1 : ('a -> 'r) encoder
  val fun2 : ('a -> 'b -> 'r) encoder
  val fun3 : ('a -> 'b -> 'c -> 'r) encoder
  val fun4 : ('a -> 'b -> 'c -> 'd -> 'r) encoder
  val fun5 : ('a -> 'b -> 'c -> 'd -> 'e -> 'r) encoder
  val fun6 : ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'r) encoder
  val fun7 : ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'r) encoder
  val fun8 : ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h -> 'r) encoder
  val fun9 : ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h -> 'i -> 'r) encoder
  val any : 'a encoder
end

(** {2 Value decoding} *)

type 'a decoder = js -> 'a

val decode : 'a decoder -> js -> 'a

module Decoder : sig
  val unit : unit decoder
  val int : int decoder
  val float : float decoder
  val js : js decoder
  val bool : bool decoder
  val string : string decoder
  val array : 'a decoder -> 'a array decoder
  val array_js : js array decoder
  val pair : 'a decoder -> 'b decoder -> ('a * 'b) decoder
  val triple : 'a decoder -> 'b decoder -> 'c decoder -> ('a * 'b * 'c) decoder
  val nullable : 'a decoder -> 'a option decoder
  val optional : 'a decoder -> 'a option decoder
  val field : js -> string -> 'a decoder -> 'a
  val any : 'a decoder
end

(** {2 JavaScript objects} *)

module Obj : sig
  type t = js

  val empty : unit -> t
  val of_list : (string * js) list -> t
  val of_array : (string * js) array -> t

  (** {2 Get properties} *)

  val get : t -> string -> 'a decoder -> 'a
  (** [get obj prop decoder] is the value of the property [prop] in [obj]. If [prop] is
      {!val:undefined} in [obj], {!exception:Undefined_property} is raised.

      {b Note}: [prop] must be an ASCII string. *)

  val get_opt : t -> string -> 'a decoder -> 'a option
  (** [get_opt obj prop decoder] is the value of the property [prop] in [obj] if [prop] is
      defined, and [None] otherwise.

      {b Note}: [prop] must be an ASCII string. *)

  val get_path : t -> string list -> 'a decoder -> 'a
  val get_path_opt : t -> string list -> 'a decoder -> 'a option
  val get_js : t -> js -> js

  (** {2 Set properties} *)

  val set : t -> string -> 'a encoder -> 'a -> unit
  val set_path : t -> string list -> 'a encoder -> 'a -> unit
  val set_js : t -> js -> js -> unit

  (** {2 Delete properties} *)

  val del : t -> string -> unit
  val del_js : t -> js -> unit

  (** {2 Function properties} *)

  val call0 : t -> string -> return:'r decoder -> unit -> 'r
  val call1 : t -> string -> return:'r decoder -> 'a encoder -> 'a -> 'r

  val call2 :
    t -> string -> return:'r decoder -> 'a encoder -> 'b encoder -> 'a -> 'b -> 'r

  val call3 :
       t
    -> string
    -> return:'r decoder
    -> 'a encoder
    -> 'b encoder
    -> 'c encoder
    -> 'a
    -> 'b
    -> 'c
    -> 'r

  val call4 :
       t
    -> string
    -> return:'r decoder
    -> 'a encoder
    -> 'b encoder
    -> 'c encoder
    -> 'd encoder
    -> 'a
    -> 'b
    -> 'c
    -> 'd
    -> 'r

  val call5 :
       t
    -> string
    -> return:'r decoder
    -> 'a encoder
    -> 'b encoder
    -> 'c encoder
    -> 'd encoder
    -> 'e encoder
    -> 'a
    -> 'b
    -> 'c
    -> 'd
    -> 'e
    -> 'r

  val call6 :
       t
    -> string
    -> return:'r decoder
    -> 'a encoder
    -> 'b encoder
    -> 'c encoder
    -> 'd encoder
    -> 'e encoder
    -> 'f encoder
    -> 'a
    -> 'b
    -> 'c
    -> 'd
    -> 'e
    -> 'f
    -> 'r

  val call7 :
       t
    -> string
    -> return:'r decoder
    -> 'a encoder
    -> 'b encoder
    -> 'c encoder
    -> 'd encoder
    -> 'e encoder
    -> 'f encoder
    -> 'g encoder
    -> 'a
    -> 'b
    -> 'c
    -> 'd
    -> 'e
    -> 'f
    -> 'g
    -> 'r

  val call8 :
       t
    -> string
    -> return:'r decoder
    -> 'a encoder
    -> 'b encoder
    -> 'c encoder
    -> 'd encoder
    -> 'e encoder
    -> 'f encoder
    -> 'g encoder
    -> 'h encoder
    -> 'a
    -> 'b
    -> 'c
    -> 'd
    -> 'e
    -> 'f
    -> 'g
    -> 'h
    -> 'r

  val call9 :
       t
    -> string
    -> return:'r decoder
    -> 'a encoder
    -> 'b encoder
    -> 'c encoder
    -> 'd encoder
    -> 'e encoder
    -> 'f encoder
    -> 'g encoder
    -> 'h encoder
    -> 'i encoder
    -> 'a
    -> 'b
    -> 'c
    -> 'd
    -> 'e
    -> 'f
    -> 'g
    -> 'h
    -> 'i
    -> 'r

  val call0_unit : t -> string -> unit -> unit
  val call1_unit : t -> string -> 'a encoder -> 'a -> unit
  val call2_unit : t -> string -> 'a encoder -> 'b encoder -> 'a -> 'b -> unit

  val call3_unit :
    t -> string -> 'a encoder -> 'b encoder -> 'c encoder -> 'a -> 'b -> 'c -> unit

  val call4_unit :
       t
    -> string
    -> 'a encoder
    -> 'b encoder
    -> 'c encoder
    -> 'd encoder
    -> 'a
    -> 'b
    -> 'c
    -> 'd
    -> unit

  val call5_unit :
       t
    -> string
    -> 'a encoder
    -> 'b encoder
    -> 'c encoder
    -> 'd encoder
    -> 'e encoder
    -> 'a
    -> 'b
    -> 'c
    -> 'd
    -> 'e
    -> unit

  val call6_unit :
       t
    -> string
    -> 'a encoder
    -> 'b encoder
    -> 'c encoder
    -> 'd encoder
    -> 'e encoder
    -> 'f encoder
    -> 'a
    -> 'b
    -> 'c
    -> 'd
    -> 'e
    -> 'f
    -> unit

  val call7_unit :
       t
    -> string
    -> 'a encoder
    -> 'b encoder
    -> 'c encoder
    -> 'd encoder
    -> 'e encoder
    -> 'f encoder
    -> 'g encoder
    -> 'a
    -> 'b
    -> 'c
    -> 'd
    -> 'e
    -> 'f
    -> 'g
    -> unit

  val call8_unit :
       t
    -> string
    -> 'a encoder
    -> 'b encoder
    -> 'c encoder
    -> 'd encoder
    -> 'e encoder
    -> 'f encoder
    -> 'g encoder
    -> 'h encoder
    -> 'a
    -> 'b
    -> 'c
    -> 'd
    -> 'e
    -> 'f
    -> 'g
    -> 'h
    -> unit

  val call9_unit :
       t
    -> string
    -> 'a encoder
    -> 'b encoder
    -> 'c encoder
    -> 'd encoder
    -> 'e encoder
    -> 'f encoder
    -> 'g encoder
    -> 'h encoder
    -> 'i encoder
    -> 'a
    -> 'b
    -> 'c
    -> 'd
    -> 'e
    -> 'f
    -> 'g
    -> 'h
    -> 'i
    -> unit

  val call_js : t -> string -> t array -> js
  val call_js_unit : t -> string -> t array -> unit

  (** {2 New object instances} *)

  val new0 : t -> t
  val new1 : t -> 'a encoder -> 'a -> t
  val new2 : t -> 'a encoder -> 'b encoder -> 'a -> 'b -> t
  val new3 : t -> 'a encoder -> 'b encoder -> 'c encoder -> 'a -> 'b -> 'c -> t

  val new4 :
    t -> 'a encoder -> 'b encoder -> 'c encoder -> 'd encoder -> 'a -> 'b -> 'c -> 'd -> t

  val new5 :
       t
    -> 'a encoder
    -> 'b encoder
    -> 'c encoder
    -> 'd encoder
    -> 'e encoder
    -> 'a
    -> 'b
    -> 'c
    -> 'd
    -> 'e
    -> t

  val new6 :
       t
    -> 'a encoder
    -> 'b encoder
    -> 'c encoder
    -> 'd encoder
    -> 'e encoder
    -> 'f encoder
    -> 'a
    -> 'b
    -> 'c
    -> 'd
    -> 'e
    -> 'f
    -> t

  val new7 :
       t
    -> 'a encoder
    -> 'b encoder
    -> 'c encoder
    -> 'd encoder
    -> 'e encoder
    -> 'f encoder
    -> 'g encoder
    -> 'a
    -> 'b
    -> 'c
    -> 'd
    -> 'e
    -> 'f
    -> 'g
    -> t

  val new8 :
       t
    -> 'a encoder
    -> 'b encoder
    -> 'c encoder
    -> 'd encoder
    -> 'e encoder
    -> 'f encoder
    -> 'g encoder
    -> 'h encoder
    -> 'a
    -> 'b
    -> 'c
    -> 'd
    -> 'e
    -> 'f
    -> 'g
    -> 'h
    -> t

  val new9 :
       t
    -> 'a encoder
    -> 'b encoder
    -> 'c encoder
    -> 'd encoder
    -> 'e encoder
    -> 'f encoder
    -> 'g encoder
    -> 'h encoder
    -> 'i encoder
    -> 'a
    -> 'b
    -> 'c
    -> 'd
    -> 'e
    -> 'f
    -> 'g
    -> 'h
    -> 'i
    -> t

  val new_js : t -> t array -> t
end

(** {2 Function helpers} *)

module Fun : sig
  type t = js

  val call0 : t -> return:'r decoder -> unit -> 'r
  val call1 : t -> return:'r decoder -> 'a encoder -> 'a -> 'r
  val call2 : t -> return:'r decoder -> 'a encoder -> 'b encoder -> 'a -> 'b -> 'r

  val call3 :
       t
    -> return:'r decoder
    -> 'a encoder
    -> 'b encoder
    -> 'c encoder
    -> 'a
    -> 'b
    -> 'c
    -> 'r

  val call4 :
       t
    -> return:'r decoder
    -> 'a encoder
    -> 'b encoder
    -> 'c encoder
    -> 'd encoder
    -> 'a
    -> 'b
    -> 'c
    -> 'd
    -> 'r

  val call5 :
       t
    -> return:'r decoder
    -> 'a encoder
    -> 'b encoder
    -> 'c encoder
    -> 'd encoder
    -> 'e encoder
    -> 'a
    -> 'b
    -> 'c
    -> 'd
    -> 'e
    -> 'r

  val call6 :
       t
    -> return:'r decoder
    -> 'a encoder
    -> 'b encoder
    -> 'c encoder
    -> 'd encoder
    -> 'e encoder
    -> 'f encoder
    -> 'a
    -> 'b
    -> 'c
    -> 'd
    -> 'e
    -> 'f
    -> 'r

  val call7 :
       t
    -> return:'r decoder
    -> 'a encoder
    -> 'b encoder
    -> 'c encoder
    -> 'd encoder
    -> 'e encoder
    -> 'f encoder
    -> 'g encoder
    -> 'a
    -> 'b
    -> 'c
    -> 'd
    -> 'e
    -> 'f
    -> 'g
    -> 'r

  val call8 :
       t
    -> return:'r decoder
    -> 'a encoder
    -> 'b encoder
    -> 'c encoder
    -> 'd encoder
    -> 'e encoder
    -> 'f encoder
    -> 'g encoder
    -> 'h encoder
    -> 'a
    -> 'b
    -> 'c
    -> 'd
    -> 'e
    -> 'f
    -> 'g
    -> 'h
    -> 'r

  val call9 :
       t
    -> return:'r decoder
    -> 'a encoder
    -> 'b encoder
    -> 'c encoder
    -> 'd encoder
    -> 'e encoder
    -> 'f encoder
    -> 'g encoder
    -> 'h encoder
    -> 'i encoder
    -> 'a
    -> 'b
    -> 'c
    -> 'd
    -> 'e
    -> 'f
    -> 'g
    -> 'h
    -> 'i
    -> 'r

  val call_js : t -> js array -> js
  val call_js_unit : t -> js array -> unit
end

module Array : sig
  type 'a t

  val empty : unit -> 'a t
  val make : int -> 'a t
  val init : int -> (int -> 'a) -> 'a t
  val of_list : 'a list -> 'a t
  val length : 'a t -> int
  val get : 'a t -> int -> 'a
  val get_opt : 'a t -> int -> 'a option
  val set : 'a t -> int -> 'a -> unit
  val push : 'a t -> 'a -> unit
  val pop : 'a t -> 'a
  val pop_opt : 'a t -> 'a option
  val iter : 'a t -> ('a -> unit) -> unit
end

module Dict : sig
  type 'a t

  val empty : unit -> 'a t
  val of_list : (string * 'a) list -> 'a t
  val of_array : (string * 'a) array -> 'a t
  val get : 'a t -> string -> 'a
  val get_opt : 'a t -> string -> 'a option
  val set : 'a t -> string -> 'a -> unit
  val del : 'a t -> string -> unit
  val entries : 'a t -> (string * 'a) array
  val keys : 'a t -> string array
  val values : 'a t -> 'a array
  val map : 'a t -> ('a -> 'b) -> 'b t
  val update : 'a t -> ('a -> 'a) -> unit
  val fold_left : 'a t -> ('acc -> 'a -> 'acc) -> 'acc -> 'acc
  val iter : 'a t -> ('a -> unit) -> unit
end
