(** Library for building reactive Web interfaces.

    {3 Example}

    {[
      open Helix

      let counter () =
        let incr = Signal.make 0 in
        let count = Signal.reduce (fun total n -> total + n) 0 incr in
        let open Html in
        fragment
          [
            h2 [] [ text "Counter" ];
            p [] [ text "Compute a count." ];
            div []
              [
                button [ on_click (fun _ -> Signal.emit 1 incr) ] [ text "+" ];
                button
                  [ on_click (fun _ -> Signal.emit (-1) incr) ]
                  [ text "-" ];
                span
                  [ style [ ("margin-left", "5px") ] ]
                  [ View.show int count ];
              ];
          ]
    ]}*)

(** {1 HTML} *)

module Html : sig
  include module type of Html
  (** @inline *)
end

type html = Html.html
(** Type for representing HTML elements and text nodes. *)

type attr = Html.attr
(** Type for representing HTML attributes. *)

module Mouse : sig
  (** Mouse signals. *)

  val position : (float * float) Signal.t
end

module Time : sig
  (** Time signals. *)

  val tick : ms:int -> unit Signal.t
end

(** {1 Reactive views} *)

module View : sig
  (** {1 Dynamic HTML} *)

  val show : ('a -> Html.html) -> 'a Signal.t -> Html.html
  (** [show to_html signal] is a dynamic HTML node created from [signal] values
      using [to_html]. *)

  val each : ('a -> Html.html) -> 'a list Signal.t -> Html.html
  (** [each to_html signal] reactively renders items from [signal] with
      [to_html].

      {[
        let items = Signal.make [ 1; 2; 3 ] in
        ul [] [ each (fun item -> li [] [ int item ]) items ]
      ]} *)

  (** {1 Dynamic attributes} *)

  val bind : ('a -> Html.attr) -> 'a Signal.t -> Html.attr
  (** [bind to_attr signal] is a dynamic HTML attribute created from [signal]
      values using [to_attr].

      {[
        let style = Signal.make [ ("color", "red") ] in
        div [ View.bind Html.style style ] [ text "Hello!" ]
      ]} *)

  val toggle : on:bool Signal.t -> Html.attr -> Html.attr
  (** [toggle ~on:signal attr] toggles an attribute based on the boolaen signal
      [signal]. *)

  val conditional : on:bool Signal.t -> Html.attr
  (** [conditional on:signal] an attribute that shows the element if [signal] is
      [true]. *)

  val visible : on:bool Signal.t -> Html.attr
  (** [visible ~on:signal] is a reactive attribute that controls the [display]
      style of HTML elements. When [signal] is [false] this attribute is
      [display: none]. *)
end

val render : Stdweb.Dom.Element.t -> html -> unit
(** [render root html] renders the [html] into the [root]. *)
