(** This module provides utilities for interfacing with JavaScript values.

    Example:

    {[
      (* Bind the global document value. *)
      let document = Jx.global "document"

      let get_element_by_id id =
        (* Define decoder for the return type. *)
        let return = Jx.Decoder.(nullable js) in
        (* Bind the method call: document.getElementById(id). *)
        Jx.Obj.call1 document "getElementById" Jx.Encoder.string id ~return
    ]} *)

type t
(** JavaScript values.

    JavaScript values can be decoded using {!module:Decoder} and encoded using
    {!module:Encoder}. *)

type js = t
(** Alias for {!type:t}. *)

val null : js
(** The JavaScript [null] value. *)

val undefined : js
(** The JavaScript [undefined] value. *)

exception Undefined_property of string
(** An exception raised when an unexpected {!val:undefined} property is
    encountered. *)

val debugger : unit -> unit
(** See
    {{:https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/debugger}
      [debugger]}. *)

val log : 'a -> unit
(** See
    {{:https://developer.mozilla.org/en-US/docs/Web/API/Console/log}
      [console.log]}. *)

val is_null : js -> bool
(** [is_null js] is [js == null]. *)

val is_undefined : js -> bool
(** [is_undefined js] is [js == undefined]. *)

val is_defined : js -> bool
(** [is_defined js] is [js != undefined]. *)

(** {1 Type helpers} *)

val type_of : js -> string
(** See
    {{:https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/typeof}
      [typeof]}. *)

val instance_of : js -> constr:js -> bool
(** See
    {{:https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/instanceof}
      [instanceof]}. *)

(** {1 Equality} *)

val equal : js -> js -> bool

(** {1 Global values} *)

val global : string -> js
(** [global name] is [globalThis\[name\]]. If this evaluates to
    {!val:undefined}, {!exception:Undefined_property} is raised. *)

val global_this : js
(** See
    {{:https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/globalThis}
      [globalThis]}. *)

(** {1 Value encoding} *)

type 'a encoder = 'a -> js
(** The type for encoders of {!type:js} values. *)

val encode : 'a encoder -> 'a -> js
(** Encode an OCaml value to a {!type:js} value using an encoder. *)

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

(** {1 Value decoding} *)

type 'a decoder = js -> 'a
(** The type for decoders of {!type:js} values. *)

val decode : 'a decoder -> js -> 'a
(** Decode an OCaml value from a {!type:js} value using an decoder. *)

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

(** {1 JavaScript objects} *)

module Obj : sig
  type t = js
  (** JavaScript objects. *)

  val t : t
  (** See
      {{:https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Object}
        [Object]}. *)

  val of_js : js -> t
  val to_js : t -> js

  val empty : unit -> t
  (** [empty ()] is [{}] in JavaScript. *)

  val of_list : (string * js) list -> t
  (** Create an object from a list of entries. *)

  val of_array : (string * js) array -> t
  (** Create an object from a array of entries. *)

  (** {1 Prototype operations} *)

  val create : t -> t
  (** Create a new object using a prototype of another object. See
      {{:https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Object/create}
        [Object.create]}. *)

  val get_prototype : t -> t

  val set_prototype : t -> t -> unit
  (** Get/set the prototype of the object. See
      {{:https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Object}
        [Object.prototype]} *)

  (** {1 Get properties} *)

  val get : t -> string -> 'a decoder -> 'a
  (** [get obj prop decoder] is the value of the property [prop] in [obj]. If
      [prop] is {!val:undefined} in [obj], {!exception:Undefined_property} is
      raised.

      {b Note}: [prop] must be an ASCII string. *)

  val get_opt : t -> string -> 'a decoder -> 'a option
  (** [get_opt obj prop decoder] is the value of the property [prop] in [obj] if
      [prop] is defined, and [None] otherwise.

      {b Note}: [prop] must be an ASCII string. *)

  val get_path : t -> string list -> 'a decoder -> 'a
  val get_path_opt : t -> string list -> 'a decoder -> 'a option
  val get_js : t -> js -> js

  (** {1 Set properties} *)

  val set : t -> string -> 'a encoder -> 'a -> unit
  val set_path : t -> string list -> 'a encoder -> 'a -> unit
  val set_js : t -> js -> js -> unit

  (** {1 Delete properties} *)

  val del : t -> string -> unit
  val del_js : t -> js -> unit

  (** {1 Check properties} *)

  val has : t -> string -> bool
  val has_js : t -> js -> bool

  (** {1 Function properties} *)

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

  val call_js : t -> string -> t array -> js
  val call_js_unit : t -> string -> t array -> unit

  (** {1 New object instances} *)

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

  val new_js : t -> t array -> t
end

(** {1 Function helpers} *)

module Fun : sig
  type t = js
  (* JavaScript functions. *)

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

  val call0_unit : t -> unit -> unit
  val call1_unit : t -> 'a encoder -> 'a -> unit
  val call2_unit : t -> 'a encoder -> 'b encoder -> 'a -> 'b -> unit

  val call3_unit :
    t -> 'a encoder -> 'b encoder -> 'c encoder -> 'a -> 'b -> 'c -> unit

  val call4_unit :
    t ->
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

  val call_js : t -> js array -> js
  val call_js_unit : t -> js array -> unit
end
