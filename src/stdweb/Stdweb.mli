(** Standard web APIs.

    See {:https://developer.mozilla.org/en-US/docs/Web/API}.*)

module Console : sig
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
  val use : ('a -> unit) -> 'a t -> unit
end

module Iterator : sig
  type 'a t
  type 'a next

  val next : 'a t -> 'a next
  val next_is_done : 'a next -> bool
  val next_value : 'a next -> 'a
  val iter : ('a -> unit) -> 'a t -> unit
end

module Map : sig
  type 'a t

  val t : Jx.t
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

module Dom : sig
  module Event : sig
    type 'a kind

    module Kind : sig
      type 'a t = 'a kind
      type base = unit t

      val to_string : 'a t -> string
    end

    type 'a t
    (** The type for DOM events of kind ['k].

        See {{:https://developer.mozilla.org/en-US/docs/Web/API/Event} Event}. *)

    type target
    (** The type for event targets.

        See
        {{:https://developer.mozilla.org/en-US/docs/Web/API/EventTarget}
          EventTarget}. *)

    val target : 'a t -> target
    (** [target ev] is the target of the event [ev].

        See:
        {{:https://developer.mozilla.org/en-US/docs/Web/API/Event/target}
          [Event.target]}. *)

    val target_value : 'a t -> string

    module Target : sig
      type t = target

      val value : t -> string
      val set_value : t -> string -> unit
      val checked : t -> bool
    end

    module Input : sig
      type nonrec kind = [ `Input ] kind
      type nonrec t = kind t

      val data : t -> string
    end

    module Keyboard : sig
      type nonrec kind = [ `Keyboard ] kind
      type nonrec t = kind t

      val key : t -> string
      val code : t -> string
    end

    module Mouse : sig
      type nonrec kind = [ `Mouse ] kind

      type nonrec t = kind t
      (** See
          {{:https://developer.mozilla.org/en-US/docs/Web/API/MouseEvent}
            [MouseEvent]}. *)

      val page_x : t -> float
      val page_y : t -> float
    end

    val click : Mouse.kind
    (** See
        {{:https://developer.mozilla.org/en-US/docs/Web/API/Element/click_event}
          [click_event]}. *)

    val input : Input.kind
    (** See
        {{:https://developer.mozilla.org/en-US/docs/Web/API/HTMLElement/input_event}
          [input_event]}. *)

    val keydown : Keyboard.kind
    (** See
        {{:https://developer.mozilla.org/en-US/docs/Web/API/Element/keydown_event}
          [keydown_event]}. *)

    val change : Kind.base
    (** See
        {{:https://developer.mozilla.org/en-US/docs/Web/API/Element/change_event}
          [change_event]}. *)
  end

  module Event_target : sig
    type t
    type 'k listener = 'k Event.t -> unit

    val add_event_listener : t -> string -> 'k listener -> unit
    val remove_event_listener : t -> string -> unit
  end

  module Node : sig
    type t
    type node = t

    module List : sig
      type t

      val for_each : t -> (node -> unit) -> unit
    end

    include module type of Event_target with type t := t

    val to_js : t -> Jx.t
    val to_event_target : t -> Event_target.t
    val parent_node : t -> t option
    val child_nodes : t -> List.t
    val first_child : t -> t option
    val last_child : t -> t option
    val next_sibling : t -> t option
    val clone_node : t -> deep:bool -> t
    val append_child : parent:t -> t -> unit
    val insert_before : parent:t -> reference:t -> t -> unit
    val replace_child : parent:t -> reference:t -> t -> unit
    val remove_child : parent:t -> t -> unit
    val set_text_content : t -> string -> unit
    val get_text_content : t -> string
    val is_same_node : t -> t -> bool
  end

  module Token_list : sig
    type t

    val toggle_class : t -> string -> unit
  end

  module Element : sig
    type t

    include module type of Node with type t := t

    val to_js : t -> Jx.t
    val to_node : t -> Node.t
    val append : t -> t -> unit
    val replace_with : t -> t -> unit
    val set_attribute : t -> string -> string -> unit
    val remove_attribute : t -> string -> unit
    val class_list : t -> Token_list.t
    val replace_children : t -> t array -> unit
    val matches : Event.target -> string -> bool
    val closest : Event.target -> string -> t option
    val of_target : Event.target -> t
  end

  module Css_style_declaration : sig
    type t

    val css_text : t -> string
    val length : t -> int
    val set_property : t -> string -> string -> unit
    val get_property : t -> string -> string
    val remove_property : t -> string -> unit
  end

  module Html_element : sig
    type t

    val of_element : Element.t -> t
    val of_node : Node.t -> t
    val to_element : t -> Element.t
    val get_style : t -> Css_style_declaration.t

    (* Additional style helpers. *)
    val set_style_property : t -> string -> string -> unit
    val get_style_property : t -> string -> string
    val remove_style_property : t -> string -> unit
  end

  module Character_data : sig
    type t

    val to_node : t -> Node.t
  end

  module Text : sig
    type t

    include module type of Character_data with type t := t

    val t : Jx.t
    val to_character_data : t -> Character_data.t
  end

  module Comment : sig
    type t

    include module type of Character_data with type t := t

    val to_character_data : t -> Character_data.t
    val make : string -> t
  end

  module Document_fragment : sig
    type t

    val t : Jx.t
    val to_node : t -> Node.t
    val make : unit -> t
    val replace_children : t -> Node.t array -> unit
  end

  module Document : sig
    type t

    val this : t
    val to_node : t -> Node.t
    val get_cookies : unit -> string
    val set_cookies : string -> unit
    val get_element_by_id : string -> Element.t option
    val query_selector : string -> Element.t option
    val create_element : string -> Element.t
    val create_text_node : string -> Text.t
  end

  module Window : sig
    type t

    val this : t

    include module type of Event_target with type t := t

    val to_event_target : t -> Event_target.t
    val set_interval : (unit -> unit) -> int -> unit
    val set_timeout : (unit -> unit) -> int -> unit
  end
end

module Global : sig
  val this : Jx.t
  val document : Dom.Document.t
  val window : Dom.Window.t
end
