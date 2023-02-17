(** Reactive signals. *)

type 'a t
(** The type for signals that emit values of type ['a]. *)

(** {2 Creating signals} *)

val make : 'a -> 'a t
(** [make x] is a signal with an initial value [x]. *)

val emitter :
  ?equal:('a -> 'a -> bool) -> init:'a -> (('a -> unit) -> unit) -> 'a t
(** [emitter ?equal ~init f] is a signal with a starting value [init] and whose
    values are emitted by [f]. The emitted values will be deduplicated by
    [equal], if provided.

    Function [f] will be passed an "emit" callback as an argument that can be
    used to emit new values:

    {[
      Signal.emitter ~init:0 (fun emit ->
          emit 1;
          emit 2;
          emit 3)
    ]} *)

val reducer : ('acc -> 'a -> 'acc) -> 'acc -> 'acc t * ('a -> unit)
(** [reducer f init] is [(signal, dispatch)], that is, a reducer signal and a
    dispatch function. The signal starts with the initial accumulator value
    [init]. Whenever [dispatch] is called with a value, the accumulator is
    updated using [f] and emitted to [signal]. *)

val never : unit t
(** [never] is a constant signal of [()]. It ignores it's subscribers and
    ignores values emitted to it. *)

(** {2 Emitting values} *)

val emit : 'a -> 'a t -> unit
(** [emit x s] emits value [x] to signal [s]. The emitted value will be
    dispatched to all subscribers of [s]. *)

val update : ('a -> 'a) -> 'a t -> unit
(** [update f s] changes the current value of [s] using [f], emitting the
    result. *)

(** {2 Getting values} *)

val get : 'a t -> 'a
(** [get s] is the current value of signal [s]. *)

(** {2 Subscribing to values} *)

val use : ('a -> unit) -> 'a t -> unit
(** [use f s] permanently subscribes to [s] and immediately calls the callback
    [f] with the current value of the signal. *)

val use2 : ('a -> 'b -> unit) -> 'a t -> 'b t -> unit
(** [use2 f s1 s2] permanently subscribes to [s1] and [s2] and immediately calls
    the callback [f] with the current values of the signals. *)

val sub : ('a -> unit) -> 'a t -> unit
(** [sub f s] permanently subscribes to [s] without immediately calling [f],
    instead [f] is called when new values are emitted to [s]. *)

val sub2 : ('a -> 'b -> unit) -> 'a t -> 'b t -> unit
(** [sub2 f s1 s2] permanently subscribes to [s1] and [s2] without immediately
    calling [f], instead [f] is called when new values are emitted to [s1] or
    [s2]. *)

(** {2 Transforming signals} *)

val map : ('a -> 'b) -> 'a t -> 'b t
(** [map f s] is a signal derived from [s] with the values mapped using [f].

    Note that the resulting signal can be subsribed to independently from [s]. *)

val map2 : ('a -> 'b -> 'r) -> 'a t -> 'b t -> 'r t
(** [map f s1 s2] is a signal that combines the values from [s1] and [s2] using
    [f]. *)

val map3 : ('a -> 'b -> 'c -> 'r) -> 'a t -> 'b t -> 'c t -> 'r t
(** [map f s1 s2 s3] is a signal that combines the values from [s1], [s2] and
    [s3] using [f]. *)

val const : 'a -> _ t -> 'a t
(** [const x s] is a constant signal transformer for [s]. That is, it will
    {e always} emits [x] whenever values are emitted to [s]. *)

val tap : ('a -> unit) -> 'a t -> 'a t
(** [tap f s] intercepts all values emitted by signal [s] and calls the
    effectful function [f] with the value. *)

val filter : ('a -> bool) -> seed:'a -> 'a t -> 'a t
(** [filter pred ~seed s] is a signal derived from [s] that only emits values
    that satisfy the predicate [pred]. If the current value of [s] does not
    satisfy [pred], the value of the filtered signal is set to [seed].

    Note that the resulting signal can be subsribed to independently from [s]. *)

val filter_map : ('a -> 'b option) -> seed:'b -> 'a t -> 'b t
(** [filter_map f ~seed s] is a signal derived from [s] that emits values
    transformed with [f] that are not [None]. If the current value of [s]
    results in [None], the value of the signal is set to [seed].

    Note that the resulting signal can be subsribed to independently from [s]. *)

val reduce : ('acc -> 'a -> 'acc) -> 'acc -> 'a t -> 'acc t
(** [reduce f init s] is a signal computed from continously updating [init]
    using a reducing function [f] applied to [init] and emitted values from [s].

    Emitting values directly to this signal will reset the internal accumulator. *)

val uniq : ?equal:('a -> 'a -> bool) -> 'a t -> 'a t
(** [uniq ?equal s] is a signal that prevents emitting values to subscribers
    that are considered equal according to [equal]. By default physical equality
    is used for [equal]. *)

(** {2 Combining signals} *)

val select : 'a t list -> 'a t
(** [select l] is a signal that selects values emitted by all signals in list
    [l].

    {b Raises}: [Invalid_argument] if [l] is empty. *)

val pair : 'a t -> 'b t -> ('a * 'b) t
(** [pair s1 s2] is a signal that combines the values from [s1] and [s2] and
    emits them as pairs. *)

val triple : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
(** [triple s1 s2 s3] is a signal that combines the values from [s1], [s2] and
    [s3] and emits them as triple. *)

val apply : ('a -> 'b) t -> 'a t -> 'b t
(** [apply f_s x_s] is a signal produced from applying values from signal [x_s]
    to the functions in signal [f_s]. *)

val sample : on:_ t -> 'b t -> 'b t
(** [sample ~on:s1 s2] samples values from [s2] any time a value is emitted to
    [s1]. *)

(* Currently defined in [Signal_syntax] for compatibility with ReScript. *)
(* {2 Syntax definitions} *)

module Syntax : sig
  (** Syntax for working with signal values.

      {[
        open Signal.Syntax

        let sum_signal s1 s2 =
          let+ x1 = s1 and+ x2 = s2 in
          x1 + x2
      ]} *)

  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
  val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
  val ( <~ ) : ('a -> 'b) -> 'a t -> 'b t
  val ( ~~ ) : ('a -> 'b) t -> 'a t -> 'b t
end
