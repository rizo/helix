type t
type js = t

val null : js
(** The JavaScript [null] value. *)

val undefined : js
(** The JavaScript [undefined] value. *)

val is_null : js -> bool
(** [is_null t] is [t == null]. *)

val is_undefined : js -> bool
(** [is_undefined t] is [t == undefined]. *)

(** {2 Type helpers} *)

val repr : 'a -> t
val typeof : t -> t
val instanceof : t -> t -> bool

(** {2 Equality} *)

val equal : js -> js -> bool

(** {2 Global values} *)

val global : string -> js
(** Get a property from [globalThis]. See
    {{:https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/globalThis}
    [globalThis]}. *)

val global_this : js
(** See
    {{:https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/globalThis}
    [globalThis]}. *)

(** {2 Converters} *)

type 'a encoder = 'a -> t

val encode : 'a encoder -> 'a -> t

module Encoder : sig
  val unit : unit encoder
  val int : int encoder
  val float : float encoder
  val js : t encoder
  val bool : bool encoder
  val string : string encoder
  val array : 'a encoder -> 'a array encoder
  val js_array : js array encoder
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
end

type 'a decoder = t -> 'a

val decode : 'a decoder -> t -> 'a

module Decoder : sig
  val unit : unit decoder
  val int : int decoder
  val float : float decoder
  val js : t decoder
  val bool : bool decoder
  val string : string decoder
  val array : 'a decoder -> 'a array decoder
  val js_array : js array decoder
  val pair : 'a decoder -> 'b decoder -> ('a * 'b) decoder
  val triple : 'a decoder -> 'b decoder -> 'c decoder -> ('a * 'b * 'c) decoder
  val nullable : 'a decoder -> 'a option decoder
  val optional : 'a decoder -> 'a option decoder
  val field : t -> string -> 'a decoder -> 'a
end

(** {2 JavaScript objects} *)

module Obj : sig
  type t = js

  val empty : unit -> t
  val of_list : (string * t) list -> t
  val of_array : (string * t) array -> t

  (** {2 Get properties} *)

  val get : t -> string -> 'a decoder -> 'a option
  val get_path : t -> string list -> 'a decoder -> 'a option
  val get_js : t -> string -> js

  (** {2 Set properties} *)

  val set : t -> string -> 'a encoder -> 'a -> unit
  val set_path : t -> string list -> 'a encoder -> 'a -> unit
  val set_js : t -> string -> t -> unit

  (** {2 Delete properties} *)

  val del : t -> string -> unit

  (** {2 Function properties} *)

  val call0 : t -> string -> return:'r decoder -> unit -> 'r
  val call1 : t -> string -> return:'r decoder -> 'a encoder -> 'a -> 'r

  val call2 :
    t ->
    string ->
    return:'r decoder ->
    'a encoder ->
    'b encoder ->
    'a ->
    'b ->
    'r

  val call3 :
    t ->
    string ->
    return:'r decoder ->
    'a encoder ->
    'b encoder ->
    'c encoder ->
    'a ->
    'b ->
    'c ->
    'r

  val call4 :
    t ->
    string ->
    return:'r decoder ->
    'a encoder ->
    'b encoder ->
    'c encoder ->
    'd encoder ->
    'a ->
    'b ->
    'c ->
    'd ->
    'r

  val call5 :
    t ->
    string ->
    return:'r decoder ->
    'a encoder ->
    'b encoder ->
    'c encoder ->
    'd encoder ->
    'e encoder ->
    'a ->
    'b ->
    'c ->
    'd ->
    'e ->
    'r

  val call6 :
    t ->
    string ->
    return:'r decoder ->
    'a encoder ->
    'b encoder ->
    'c encoder ->
    'd encoder ->
    'e encoder ->
    'f encoder ->
    'a ->
    'b ->
    'c ->
    'd ->
    'e ->
    'f ->
    'r

  val call7 :
    t ->
    string ->
    return:'r decoder ->
    'a encoder ->
    'b encoder ->
    'c encoder ->
    'd encoder ->
    'e encoder ->
    'f encoder ->
    'g encoder ->
    'a ->
    'b ->
    'c ->
    'd ->
    'e ->
    'f ->
    'g ->
    'r

  val call8 :
    t ->
    string ->
    return:'r decoder ->
    'a encoder ->
    'b encoder ->
    'c encoder ->
    'd encoder ->
    'e encoder ->
    'f encoder ->
    'g encoder ->
    'h encoder ->
    'a ->
    'b ->
    'c ->
    'd ->
    'e ->
    'f ->
    'g ->
    'h ->
    'r

  val call9 :
    t ->
    string ->
    return:'r decoder ->
    'a encoder ->
    'b encoder ->
    'c encoder ->
    'd encoder ->
    'e encoder ->
    'f encoder ->
    'g encoder ->
    'h encoder ->
    'i encoder ->
    'a ->
    'b ->
    'c ->
    'd ->
    'e ->
    'f ->
    'g ->
    'h ->
    'i ->
    'r

  val call0_unit : t -> string -> unit -> unit
  val call1_unit : t -> string -> 'a encoder -> 'a -> unit
  val call2_unit : t -> string -> 'a encoder -> 'b encoder -> 'a -> 'b -> unit

  val call3_unit :
    t ->
    string ->
    'a encoder ->
    'b encoder ->
    'c encoder ->
    'a ->
    'b ->
    'c ->
    unit

  val call4_unit :
    t ->
    string ->
    'a encoder ->
    'b encoder ->
    'c encoder ->
    'd encoder ->
    'a ->
    'b ->
    'c ->
    'd ->
    unit

  val call5_unit :
    t ->
    string ->
    'a encoder ->
    'b encoder ->
    'c encoder ->
    'd encoder ->
    'e encoder ->
    'a ->
    'b ->
    'c ->
    'd ->
    'e ->
    unit

  val call6_unit :
    t ->
    string ->
    'a encoder ->
    'b encoder ->
    'c encoder ->
    'd encoder ->
    'e encoder ->
    'f encoder ->
    'a ->
    'b ->
    'c ->
    'd ->
    'e ->
    'f ->
    unit

  val call7_unit :
    t ->
    string ->
    'a encoder ->
    'b encoder ->
    'c encoder ->
    'd encoder ->
    'e encoder ->
    'f encoder ->
    'g encoder ->
    'a ->
    'b ->
    'c ->
    'd ->
    'e ->
    'f ->
    'g ->
    unit

  val call8_unit :
    t ->
    string ->
    'a encoder ->
    'b encoder ->
    'c encoder ->
    'd encoder ->
    'e encoder ->
    'f encoder ->
    'g encoder ->
    'h encoder ->
    'a ->
    'b ->
    'c ->
    'd ->
    'e ->
    'f ->
    'g ->
    'h ->
    unit

  val call9_unit :
    t ->
    string ->
    'a encoder ->
    'b encoder ->
    'c encoder ->
    'd encoder ->
    'e encoder ->
    'f encoder ->
    'g encoder ->
    'h encoder ->
    'i encoder ->
    'a ->
    'b ->
    'c ->
    'd ->
    'e ->
    'f ->
    'g ->
    'h ->
    'i ->
    unit

  val call_js : t -> string -> t array -> t
  val call_js_unit : t -> string -> t array -> unit

  (** {2 New object instances} *)

  val new0 : t -> t
  val new1 : t -> 'a encoder -> 'a -> t
  val new2 : t -> 'a encoder -> 'b encoder -> 'a -> 'b -> t
  val new3 : t -> 'a encoder -> 'b encoder -> 'c encoder -> 'a -> 'b -> 'c -> t

  val new4 :
    t ->
    'a encoder ->
    'b encoder ->
    'c encoder ->
    'd encoder ->
    'a ->
    'b ->
    'c ->
    'd ->
    t

  val new5 :
    t ->
    'a encoder ->
    'b encoder ->
    'c encoder ->
    'd encoder ->
    'e encoder ->
    'a ->
    'b ->
    'c ->
    'd ->
    'e ->
    t

  val new6 :
    t ->
    'a encoder ->
    'b encoder ->
    'c encoder ->
    'd encoder ->
    'e encoder ->
    'f encoder ->
    'a ->
    'b ->
    'c ->
    'd ->
    'e ->
    'f ->
    t

  val new7 :
    t ->
    'a encoder ->
    'b encoder ->
    'c encoder ->
    'd encoder ->
    'e encoder ->
    'f encoder ->
    'g encoder ->
    'a ->
    'b ->
    'c ->
    'd ->
    'e ->
    'f ->
    'g ->
    t

  val new8 :
    t ->
    'a encoder ->
    'b encoder ->
    'c encoder ->
    'd encoder ->
    'e encoder ->
    'f encoder ->
    'g encoder ->
    'h encoder ->
    'a ->
    'b ->
    'c ->
    'd ->
    'e ->
    'f ->
    'g ->
    'h ->
    t

  val new9 :
    t ->
    'a encoder ->
    'b encoder ->
    'c encoder ->
    'd encoder ->
    'e encoder ->
    'f encoder ->
    'g encoder ->
    'h encoder ->
    'i encoder ->
    'a ->
    'b ->
    'c ->
    'd ->
    'e ->
    'f ->
    'g ->
    'h ->
    'i ->
    t

  val new_js : js -> js array -> t
end

(** {2 Function helpers} *)

module Fun : sig
  type t = js

  val call0 : t -> return:'r decoder -> unit -> 'r
  val call1 : t -> return:'r decoder -> 'a encoder -> 'a -> 'r

  val call2 :
    t -> return:'r decoder -> 'a encoder -> 'b encoder -> 'a -> 'b -> 'r

  val call3 :
    t ->
    return:'r decoder ->
    'a encoder ->
    'b encoder ->
    'c encoder ->
    'a ->
    'b ->
    'c ->
    'r

  val call4 :
    t ->
    return:'r decoder ->
    'a encoder ->
    'b encoder ->
    'c encoder ->
    'd encoder ->
    'a ->
    'b ->
    'c ->
    'd ->
    'r

  val call5 :
    t ->
    return:'r decoder ->
    'a encoder ->
    'b encoder ->
    'c encoder ->
    'd encoder ->
    'e encoder ->
    'a ->
    'b ->
    'c ->
    'd ->
    'e ->
    'r

  val call6 :
    t ->
    return:'r decoder ->
    'a encoder ->
    'b encoder ->
    'c encoder ->
    'd encoder ->
    'e encoder ->
    'f encoder ->
    'a ->
    'b ->
    'c ->
    'd ->
    'e ->
    'f ->
    'r

  val call7 :
    t ->
    return:'r decoder ->
    'a encoder ->
    'b encoder ->
    'c encoder ->
    'd encoder ->
    'e encoder ->
    'f encoder ->
    'g encoder ->
    'a ->
    'b ->
    'c ->
    'd ->
    'e ->
    'f ->
    'g ->
    'r

  val call8 :
    t ->
    return:'r decoder ->
    'a encoder ->
    'b encoder ->
    'c encoder ->
    'd encoder ->
    'e encoder ->
    'f encoder ->
    'g encoder ->
    'h encoder ->
    'a ->
    'b ->
    'c ->
    'd ->
    'e ->
    'f ->
    'g ->
    'h ->
    'r

  val call9 :
    t ->
    return:'r decoder ->
    'a encoder ->
    'b encoder ->
    'c encoder ->
    'd encoder ->
    'e encoder ->
    'f encoder ->
    'g encoder ->
    'h encoder ->
    'i encoder ->
    'a ->
    'b ->
    'c ->
    'd ->
    'e ->
    'f ->
    'g ->
    'h ->
    'i ->
    'r

  val call_js : t -> t array -> t
  val call_js_unit : t -> t array -> unit
end

module Dict : sig
  type t = Obj.t

  val entries : js -> (string * js) array
end
