(** Library for building reactive UI components. *)

(** {1 HTML} *)

type html = Html.html
(** Type for representing HTML elements and text nodes. *)

type attr = Html.attr
(** Type for representing HTML attributes. *)

module Html : sig
  include module type of Html with type html = html
  (** @inline *)
end

(** {1 Signals} *)

type 'a signal = 'a Signal.t
(** Type for reactive signals. *)

module Signal : sig
  include module type of Signal with type 'a t = 'a signal
  (** @inline *)
end

module Mouse : sig
  (** Mouse signals. *)

  val position : (float * float) signal
end

module Time : sig
  (** Time signals. *)

  val tick : ms:int -> unit signal
end

(** {1 Reactive views} *)

module View : sig
  val show : ('a -> Html.html) -> 'a Signal.t -> Html.html
  (** [show to_html signal] is a dynamic HTML node created from [signal] values
      using [to_html]. *)

  (* val show_conditional :
     ?on:bool Signal.t -> ('a -> Html.html) -> 'a Signal.t -> Html.html *)
  (** [show ?on:condition to_html signal] is a reactive HTML element created
      from [signal] values using [to_html]. If boolean [condition] signal is
      passed the resulting element is only rendered if the signal is [true]. *)

  val each : ('a -> Html.html) -> 'a list Signal.t -> Html.html
  (** [each to_html signal] reactively renders items from [signal] with
      [to_html].

      {[
        let items = Signal.make [ 1; 2; 3 ] in
        ul [] [ each (fun item -> li [] [ int item ]) items ]
      ]} *)

  (* val option : ('a -> Html.html) -> 'a option Signal.t -> Html.html *)
  (** [option to_html items] reactively renders [items] with [to_html].

      {[
        let name = Signal.make None in
        div [] [ optional string name ]
      ]} *)

  (* val filter : ('a -> bool) -> 'a Signal.t -> Html.html -> Html.html *)
  (* val conditional : on:bool Signal.t -> 'a Signal.t -> Html.html *)
  (* val on : bool Signal.t -> Html.html -> Html.html *)

  val conditional_attr : bool Signal.t -> Html.attr

  (* val conditional_html : on:bool Signal.t -> Html.html -> Html.html *)
  (** [conditional ~on:signal html] shows [html] when [signal] is [true]. *)

  (* val optional : ('a -> Html.html option) -> 'a Signal.t -> Html.html *)

  val assign : Html.attr Signal.t -> Html.attr
  (** [attr attr_signal] dynamically binds the attributes produced by
      [attr_signal] to an element.

      {[
        let attr = Signal.make (Html.style_list [ ("color", "red") ]) in
        div [ View.assign attr ] [ text "Hello!" ]
      ]} *)

  val bind : ('a -> Html.attr) -> 'a Signal.t -> Html.attr
  (** [bind to_attr signal] is a dynamic HTML attribute created from [signal]
      values using [to_attr]. *)

  val visible : on:bool Signal.t -> Html.attr
  (** [visible ~on:signal] is a reactive attribute that controls the [display]
      style of HTML elements. When [signal] is [false] this attribute is
      [display: none]. *)

  val toggle : on:bool Signal.t -> Html.attr -> Html.attr
  (** [toggle ~on:signal attr] toggles an attribute based on the boolaen signal
      [signal]. *)
end

val render : Stdweb.Dom.Element.t -> html -> unit
(** [render root_elem html] renders the [html] into the [root_elem]. *)
