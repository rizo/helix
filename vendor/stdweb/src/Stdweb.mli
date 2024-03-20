(** Standard web APIs.

    See {:https://developer.mozilla.org/en-US/docs/Web/API}.*)

module Console : sig
  type t

  val t : Jx.t
  (** See
      {{:https://developer.mozilla.org/en-US/docs/Web/API/console} [console]}. *)

  val log : 'a -> unit
  (** See
      {{:https://developer.mozilla.org/en-US/docs/Web/API/console/log} [log]}. *)

  val error : 'a -> unit
  (** See
      {{:https://developer.mozilla.org/en-US/docs/Web/API/console/error}
        [error]}. *)

  val info : 'a -> unit
  (** See
      {{:https://developer.mozilla.org/en-US/docs/Web/API/console/info} [info]}. *)

  val warn : 'a -> unit
  (** See
      {{:https://developer.mozilla.org/en-US/docs/Web/API/console/warn} [warn]}. *)

  val ensure : bool -> 'a -> unit
  (** See
      {{:https://developer.mozilla.org/en-US/docs/Web/API/console/assert}
        [assert]}. *)
end

module Array : sig
  type 'a t

  val of_js : Jx.t -> 'a t
  val to_js : 'a t -> Jx.t
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
  val slice : 'a t -> int -> int -> 'a t
  val concat : 'a t -> 'a t -> 'a t
end

module Dict : sig
  type 'a t

  val of_js : Jx.t -> 'a t
  val to_js : 'a t -> Jx.t
  val of_obj : Jx.Obj.t -> 'a t
  val to_obj : 'a t -> Jx.Obj.t
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

module Promise : sig
  type 'a t
  type ('a, 'err) executor = ('a -> unit) -> ('err -> unit) -> unit

  val t : Jx.t
  val of_js : Jx.t -> 'a t
  val to_js : 'a t -> Jx.t
  val make : ('a, 'err) executor -> 'a t
  val resolve : 'a -> 'a t
  val reject : 'err -> 'a t
  val and_then : ('a -> 'b t) -> 'a t -> 'b t
  val map : ('a -> 'b) -> 'a t -> 'b t
  val use : ('a -> unit) -> 'a t -> unit
  val catch : ('err -> unit) -> 'a t -> unit
  val ignore : unit t -> unit

  module Syntax : sig
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  end
end

module Iterator : sig
  type 'a t
  type 'a next

  val next : 'a t -> 'a next
  val next_is_done : 'a next -> bool
  val next_value : 'a next -> 'a
  val iter : ('a -> unit) -> 'a t -> unit
  val unsafe_of_js : Jx.t -> Jx.t t
end

module Map : sig
  type 'a t

  val t : Jx.t
  (** The constructor object for type ['a t]. *)

  val of_js : Jx.t -> 'a t
  val to_js : 'a t -> Jx.t
  val make : unit -> 'a t
  val clear : 'a t -> unit
  val set : 'a t -> Jx.t -> 'a -> unit
  val get : 'a t -> Jx.t -> 'a
  val delete : 'a t -> Jx.t -> unit
  val keys : 'a t -> Jx.t Iterator.t
  val size : 'a t -> int
  val values : 'a t -> 'a Iterator.t
  val first_key : 'a t -> Jx.t option
end

module Object : sig
  type t = Jx.t

  val from_entries : Jx.t -> t
end

module Number : sig
  type t = float

  val to_precision : t -> int -> string
end

module Dom : sig
  type node
  type event

  (** {2 CSS styles} *)

  module Style : sig
    type t

    val text : t -> string
    val length : t -> int
    val set : t -> string -> string -> unit
    val unset : t -> string -> unit
    val get : t -> string -> string
  end

  (** {2 DOM events} *)

  module Event : sig
    type name
    (** The name of the event. *)

    module Name : sig
      type t = name

      val make : string -> name
      val to_string : name -> string
    end

    type t = event
    (** DOM events.

        See {{:https://developer.mozilla.org/en-US/docs/Web/API/Event} Event}. *)

    val target : t -> node
    (** [target ev] is the target of the event [ev].

        See:
        {{:https://developer.mozilla.org/en-US/docs/Web/API/Event/target}
          [Event.target]}. *)

    val prevent_default : t -> unit
    (** See
        {{:https://developer.mozilla.org/en-US/docs/Web/API/Event/preventDefault}
          [preventDefault]} *)

    (** {2 Generic events} *)

    val change : name
    (** See
        {{:https://developer.mozilla.org/en-US/docs/Web/API/HTMLElement/change_event}
          [change_event]}. *)

    (** {2 UI events} *)

    val resize : name
    (** See
        {{:https://developer.mozilla.org/en-US/docs/Web/API/Window/resize_event}
          [resize_event]}. *)

    (** {2 Mouse events}

        See
        {{:https://developer.mozilla.org/en-US/docs/Web/API/MouseEvent}
          [MouseEvent]}. *)

    val click : name

    val dblclick : name
    (** See
        {{:https://developer.mozilla.org/en-US/docs/Web/API/Element/dblclick_event}
          [dblclick_event]}. *)

    val mousemove : name
    val page_x : t -> float
    val page_y : t -> float

    (** {2 Keyboard events}

        See
        {{:https://developer.mozilla.org/en-US/docs/Web/API/Element/keydown_event}
          [keydown_event]}. *)

    val keydown : name
    val key : t -> string
    val code : t -> string

    (** {2 Input events}

        See
        {{:https://developer.mozilla.org/en-US/docs/Web/API/HTMLElement/input_event}
          [input_event]}. *)

    val input : name
    (** See
        {{:https://developer.mozilla.org/en-US/docs/Web/API/HTMLElement/input_event}
          [input_event]}. *)

    val before_input : name
    (** See
        {{:https://developer.mozilla.org/en-US/docs/Web/API/HTMLElement/beforeinput_event}
          [beforeinput_event]}. *)

    val cancel : name
    (** {See {:https://developer.mozilla.org/en-US/docs/Web/API/HTMLElement/cancel_event} [cancel_event]}. *)

    val data : t -> string

    (** {2 Fullscreen events} *)

    val fullscreen_change : name
    (** See
        {:https://developer.mozilla.org/en-US/docs/Web/API/Element/fullscreenchange_event}. *)

    val fullscreen_error : name
    (** See
        {:https://developer.mozilla.org/en-US/docs/Web/API/Element/fullscreenerror_event}. *)

    (** {2 Pop state events} *)

    val popstate : name
    (** See
        {:https://developer.mozilla.org/en-US/docs/Web/API/Window/popstate_event}. *)

    (** {2 Hash change event} *)

    val hashchange : name
    (** See
        {:https://developer.mozilla.org/en-US/docs/Web/API/Window/hashchange_event}. *)

    (** {2 Focus events} *)

    val blur : name
    (** See
        {:https://developer.mozilla.org/en-US/docs/Web/API/Element/blur_event}. *)

    val focusin : name
    (** See
        {:https://developer.mozilla.org/en-US/docs/Web/API/Element/focusin_event}. *)

    val focusout : name
    (** See
        {:https://developer.mozilla.org/en-US/docs/Web/API/Element/focusout_event}. *)

    (** {2 Submit events} *)
    val submit : name
    (** See
        {:https://developer.mozilla.org/en-US/docs/Web/API/HTMLFormElement/submit_event}. *)
  end

  (** {2 DOM nodes} *)

  module Text : sig
    type t = node

    val t : Jx.t
  end

  module Comment : sig
    type t = node

    val make : string -> t
  end

  module Fragment : sig
    type t = node

    val t : Jx.t
    val make : unit -> t
    val replace_children : t -> node array -> unit
  end

  module Token_list : sig
    type t

    val toggle : t -> string -> unit
    val add : t -> string -> unit
    val remove : t -> string -> unit
  end

  module Blob : sig
    type t

    val size : t -> int
    val type' : t -> string
    val text : t -> string Promise.t
  end

  module File : sig
    type t

    val last_modified : t -> float
    val name : t -> string
    val to_blob : t -> Blob.t
  end

  module Node : sig
    type t = node

    (** {2 Conversions} *)

    val to_js : t -> Jx.t

    (** {2 Traversal} *)

    val parent : t -> t option
    val children : t -> t list
    val iter_children : t -> (t -> unit) -> unit
    val first_child : t -> t option
    val last_child : t -> t option
    val next_sibling : t -> t option

    (** {2 Node operations} *)

    val clone_node : t -> deep:bool -> t
    val is_same_node : t -> t -> bool
    val remove : t -> unit
    val replace_with : t -> t -> unit

    (** {2 Children manipulation} *)

    val append : t -> t -> unit
    val append_child : parent:t -> t -> unit
    val remove_child : parent:t -> t -> unit
    val insert_before : parent:t -> reference:t -> t -> unit
    val replace_child : parent:t -> reference:t -> t -> unit
    val replace_children : t -> t array -> unit
    val insert_before_begin : reference:t -> t -> unit
    val insert_after_begin : reference:t -> t -> unit
    val insert_before_end : reference:t -> t -> unit
    val insert_after_end : reference:t -> t -> unit

    (** {2 Text content} *)

    val get_text_content : t -> string
    val set_text_content : t -> string -> unit

    (* {2 HTML content} *)

    val set_inner_html : t -> string -> unit

    (** {2 Event handling} *)

    val bind : t -> Event.name -> (Event.t -> unit) -> unit
    val unbind : t -> Event.name -> (Event.t -> unit) -> unit

    (** {2 State changes} *)

    val blur : t -> unit
    (** Removes keyboard focus from the currently focused element. *)

    val click : t -> unit
    (** Sends a mouse click event to the element. *)

    val focus : t -> unit
    (** Makes the element the current keyboard focus. *)

    (** {2 Popover operations} *)

    val hide_popover : t -> unit
    (** Hides a popover element by removing it from the top layer and styling it
        with display: none. *)

    val show_popover : t -> unit
    (** Shows a popover element by adding it to the top layer and removing
        display: none; from its styles. *)

    val toggle_popover : t -> unit
    (** Toggles a popover element between the hidden and showing states. *)

    (** {3 Selectors} *)

    val matches : t -> string -> bool
    val closest : t -> string -> t option

    (** {3 Input methods} *)

    val select : t -> unit

    (** {3 Anchor methods} *)

    val to_string : t -> string

    (** {3 Attributes} *)

    val get_class_list : t -> Token_list.t
    val set_attr : t -> string -> string -> unit
    val get_attr : t -> string -> string
    val unset_attr : t -> string -> unit
    val set_style : node -> string -> unit
    val get_style : node -> Style.t
    val reset_style : t -> unit
    val set_disabled : t -> bool -> unit
    val get_href : t -> string
    val set_href : t -> string -> unit
    val get_value : t -> string
    val set_value : t -> string -> unit
    val reset_value : t -> unit
    val set_autofocus : t -> bool -> unit
    val get_offset_width : t -> int
    val get_checked : t -> bool
    val set_checked : t -> bool -> unit
    val get_files : t -> File.t array
  end

  module Document : sig
    type t

    val this : t
    val get_cookies : unit -> string
    val set_cookies : string -> unit
    val get_element_by_id : string -> node option
    val query_selector : string -> node option
    val query : string -> node option
    val create_element : string -> node
    val create_text_node : string -> Text.t
    val active_element : t -> Node.t
  end

  module Location : sig
    type t

    val hash : t -> string
    val set_hash : t -> string -> unit
    val href : t -> string
    val set_href : t -> string -> unit
    val replace : t -> string -> unit
    val assign : t -> string -> unit
    val reload : t -> 'a
    val hostname : t -> string
    val search : t -> string
  end

  module Window : sig
    type t

    val this : t
    val location : Location.t
    val set_interval : (unit -> unit) -> int -> unit
    val set_timeout : (unit -> unit) -> int -> int
    val clear_timeout : int -> unit
    val confirm : string -> bool
    val prompt : string -> string option
    val alert : string -> unit

    (** {2 Event handling} *)

    val bind : Event.name -> (Event.t -> unit) -> unit
    val unbind : Event.name -> unit
  end
end

module Json : sig
  type t = Jx.t

  val stringify : ?replacer:t -> ?space:t -> t -> string
  (** See
      {{:https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/JSON/stringify}
        [stringify]}. *)

  val parse : ?reviver:t -> string -> t
  (** See
      {{:https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/JSON/parse}
        [parse]}. *)
end

module Form_data : sig
  type t

  val constr : Jx.t
  val make : ?submitter:Dom.node -> Dom.node -> t
  val to_json : t -> Json.t
  val to_js : t -> Jx.t
  val to_assoc : t -> (string * string) list
end

module Fetch : sig
  type headers

  module Headers : sig
    type t = headers

    val entries : t -> Jx.t Iterator.t
  end

  type response

  module Response : sig
    type t = response

    val ok : t -> bool
    val status : t -> int
    val text : t -> string Promise.t
    val json : t -> Jx.t Promise.t
    val headers : t -> headers
  end

  module Body : sig
    type t

    val of_string : string -> t
    val of_form_data : Form_data.t -> t
    val to_js : t -> Jx.t
    val unsafe_of_js : Jx.t -> t
  end

  type meth = [ `Get | `Put | `Post ]
  type mode = [ `Cors | `No_cors ]

  val fetch :
    ?body:Body.t ->
    ?meth:meth ->
    ?headers:(string * string) list ->
    ?mode:mode ->
    string ->
    response Promise.t
end

module Storage : sig
  type t

  val local : t
  val get : t -> string -> string option
  val set : t -> string -> string -> unit
  val del : t -> string -> unit
end

(** See
    {{:https://developer.mozilla.org/en-US/docs/Web/API/Clipboard} [Clipboard]}.

    Example:

    {[
      open Stdweb

      let clipboard = Navigator.clipboard navigator in
      Clipboard.write_text clipboard "Hello world"
      |> Promise.ignore
    ]} *)
module Clipboard : sig
  type t

  val write_text : t -> string -> unit Promise.t
  (** See
      {{:https://developer.mozilla.org/en-US/docs/Web/API/Clipboard/writeText}
        [writeText]}. *)
end

module Websocket : sig
  type t

  module Event : sig
    (** {2 Message events} *)

    val data : Dom.Event.t -> string
    (** See
        {{:https://developer.mozilla.org/en-US/docs/Web/API/MessageEvent/data}
          [MessageEvent: data]}.*)

    val open' : Dom.Event.name
    (** {2 WebSocket event names} *)

    val message : Dom.Event.name
    val close : Dom.Event.name
    val error : Dom.Event.name
  end

  val make : string -> t
  val close : ?code:int -> ?reason:string -> t -> unit
  val send : t -> string -> unit
  val bind : t -> Dom.Event.name -> (Dom.Event.t -> unit) -> unit
  val unbind : t -> Dom.Event.name -> unit
end

module Url_search_params : sig
  type t

  val of_string : string -> t
  val make : (string * string) list -> t
  val empty : unit -> t

  val get : t -> string -> string option
  (** See
      {{:https://developer.mozilla.org/en-US/docs/Web/API/URLSearchParams/get}
        [get]}. *)

  val delete : t -> ?value:string -> string -> unit
  (** See
      {{:https://developer.mozilla.org/en-US/docs/Web/API/URLSearchParams/delete}
        [delete]}. *)

  val has : t -> ?value:string -> string -> bool
  (** See
      {{:https://developer.mozilla.org/en-US/docs/Web/API/URLSearchParams/has}
        [has]}. *)

  val to_string : t -> string
  (** See
      {{:https://developer.mozilla.org/en-US/docs/Web/API/URLSearchParams/toString}
        [toString]}. *)
end

module Navigator : sig
  type t

  val clipboard : t -> Clipboard.t
end

(** {2 Global values} *)

val document : Dom.Document.t
val window : Dom.Window.t
val console : Console.t
val navigator : Navigator.t

val encode_uri_component : string -> string
(** See
    {{:https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/encodeURIComponent}
      [encodeURIComponent]}. *)
