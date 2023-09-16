module Global = Stdweb_global

type node = Jx.t
type event = Jx.t

(* Style *)

type style = Jx.t

module Style = struct
  type t = style

  let text this = Jx.Obj.get this "cssText" Jx.Decoder.string
  let length this = Jx.Obj.get this "length" Jx.Decoder.int

  let set this name value =
    Jx.Obj.call_js_unit this "setProperty"
      [| Jx.Encoder.string name; Jx.Encoder.string value |]

  let unset target name =
    Jx.Obj.call_js_unit target "removeProperty" [| Jx.Encoder.string name |]

  let get this name =
    Jx.Decoder.string
      (Jx.Obj.call_js this "setProperty" [| Jx.Encoder.string name |])
end

(* DOM events *)

module Event = struct
  type name = Jx.t

  module Name = struct
    type t = Jx.t

    let to_string = Jx.Decoder.string
    let make = Jx.Encoder.string
  end

  type t = event

  let target t = Jx.Obj.get t "target" Jx.Decoder.any

  (* Generic events *)

  let change = Name.make "change"

  (* UI events *)

  let resize = Name.make "resize"

  (* Mouse *)

  let click = Jx.Encoder.string "click"
  let mousemove = Jx.Encoder.string "mousemove"
  let page_x this = Jx.Obj.get this "pageX" Jx.Decoder.float
  let page_y this = Jx.Obj.get this "pageY" Jx.Decoder.float

  (* Keyboard events *)

  let keydown = Name.make "keydown"
  let key this = Jx.Obj.get this "key" Jx.Decoder.string
  let code this = Jx.Obj.get this "keyCode" Jx.Decoder.string

  (* Input *)

  let before_input = Name.make "beforeinput"
  let input = Name.make "input"
  let data this = Jx.Obj.get this "data" Jx.Decoder.string

  (* Fullscreen events *)
  let fullscreen_change = Name.make "fullscreenchange"
  let fullscreen_error = Name.make "fullscreenerror"
end

module Node = struct
  type t = node

  (* Conversions *)

  let to_js t = t

  (* Traversal *)

  let parent this = Jx.Obj.get this "parentNode" Jx.Decoder.(nullable js)
  let first_child this = Jx.Obj.get this "firstChild" Jx.Decoder.(nullable js)
  let last_child this = Jx.Obj.get this "lastChild" Jx.Decoder.(nullable js)
  let next_sibling this = Jx.Obj.get this "nextSibling" Jx.Decoder.(nullable js)

  let iter_children this f =
    let node_list = Jx.Obj.get this "childNodes" Jx.Decoder.js in
    Jx.Obj.call_js_unit node_list "forEach" [| Jx.Encoder.fun1 f |]

  let children this =
    let acc = ref [] in
    iter_children this (fun c -> acc := c :: !acc);
    List.rev !acc

  (* Node operations *)

  let clone_node this ~deep =
    Jx.Obj.call1 this "cloneNode" Jx.Encoder.bool deep ~return:Jx.Decoder.js

  let is_same_node this other =
    Jx.Decoder.bool (Jx.Obj.call_js this "isSameNode" [| other |])

  let remove this = Jx.Obj.call0_unit this "remove" ()

  let replace_with this other =
    Jx.Obj.call_js_unit this "replaceWith" [| other |]

  (* Children manipulation *)

  let append this other = Jx.Obj.call_js_unit this "append" [| other |]

  let append_child ~parent other =
    Jx.Obj.call_js_unit parent "appendChild" [| other |]

  let remove_child ~parent other =
    Jx.Obj.call_js_unit parent "removeChild" [| other |]

  let insert_before ~parent ~reference new_node =
    Jx.Obj.call_js_unit parent "insertBefore" [| new_node; reference |]

  let replace_child ~parent ~reference new_node =
    Jx.Obj.call_js_unit parent "replaceChild" [| new_node; reference |]

  let replace_children this children =
    Jx.Obj.call_js_unit this "replaceChildren" children

  (* Text content *)

  let set_text_content this text =
    Jx.Obj.set this "textContent" Jx.Encoder.string text

  let get_text_content this = Jx.Obj.get this "textContent" Jx.Decoder.string

  (* Event handling *)

  let bind this event_type f =
    Jx.Obj.call2_unit this "addEventListener" Jx.Encoder.js Jx.Encoder.fun1
      event_type f

  let unbind this event_type =
    Jx.Obj.call_js_unit this "removeEventLister" [| Jx.Encoder.js event_type |]

  (* State changes *)

  let blur this = Jx.Obj.call0_unit this "blur" ()
  let click this = Jx.Obj.call0_unit this "click" ()
  let focus this = Jx.Obj.call0_unit this "focus" ()

  (* Popover operations *)

  let hide_popover this = Jx.Obj.call0_unit this "hidePopover" ()
  let show_popover this = Jx.Obj.call0_unit this "showPopover" ()
  let toggle_popover this = Jx.Obj.call0_unit this "togglePopover" ()

  (* Selectors *)

  let matches this selectors =
    Jx.Decoder.bool
      (Jx.Obj.call_js this "matches" [| Jx.Encoder.string selectors |])

  let closest this selectors =
    Jx.Decoder.nullable Jx.Decoder.any
      (Jx.Obj.call_js this "closest" [| Jx.Encoder.string selectors |])

  (* input *)

  let select this = Jx.Obj.call0_unit this "select" ()

  (* a *)

  let to_string this = Jx.Obj.call0 ~return:Jx.Decoder.string this "toString" ()

  (* Attributes *)

  let set_attr this name value =
    Jx.Obj.call_js_unit this "setAttribute"
      [| Jx.Encoder.string name; Jx.Encoder.string value |]

  let get_attr this name =
    Jx.Obj.call1 this "getAttribute" ~return:Jx.Decoder.string Jx.Encoder.string
      name

  let unset_attr this name =
    Jx.Obj.call_js_unit this "removeAttribute" [| Jx.Encoder.string name |]

  let toggle_class this name =
    let token_list = Jx.Obj.get this "classList" Jx.Decoder.any in
    Jx.Obj.call1_unit token_list "toggle" Jx.Encoder.string name

  let set_style this x = Jx.Obj.set this "style" Jx.Encoder.string x
  let get_style this = Jx.Obj.get this "style" Jx.Decoder.js
  let get_href this = Jx.Obj.get this "href" Jx.Decoder.string
  let set_href this x = Jx.Obj.set this "href" Jx.Encoder.string x
  let get_value this = Jx.Obj.get this "value" Jx.Decoder.string
  let set_value this x = Jx.Obj.set this "value" Jx.Encoder.string x
  let reset_value this = Jx.Obj.set this "value" Jx.Encoder.string ""
  let set_disabled this x = Jx.Obj.set this "disabled" Jx.Encoder.bool x
  let set_autofocus this x = Jx.Obj.set this "autofocus" Jx.Encoder.bool x
end

module Text = struct
  type t = Jx.t

  let t = Jx.global "Text"
  let to_character_data t = t
end

module Comment = struct
  type t = Jx.t

  let t = Jx.global "Comment"
  let to_character_data t = t
  let make data = Jx.Obj.new1 t Jx.Encoder.string data
end

module Fragment = struct
  type t = Jx.t

  let t = Jx.global "DocumentFragment"
  let to_node t = t
  let make () = Jx.Obj.new0 t

  let replace_children this children =
    Jx.Obj.call_js_unit this "replaceChildren" children
end

module Document = struct
  type t = Jx.t

  let this = Global.document
  let to_node this = this
  let get_cookies () = Jx.Obj.get Global.document "cookie" Jx.Decoder.string
  let set_cookies cookies = Jx.Obj.set this "cookie" Jx.Encoder.string cookies

  let get_element_by_id id =
    Jx.Decoder.nullable
      (fun x -> x)
      (Jx.Obj.call_js Global.document "getElementById"
         [| Jx.Encoder.string id |]
      )

  let query_selector selector =
    Jx.Obj.call1 Global.document "querySelector"
      ~return:Jx.Decoder.(nullable js)
      Jx.Encoder.string selector

  let query selector =
    Jx.Obj.call1 Global.document "querySelector"
      ~return:Jx.Decoder.(nullable js)
      Jx.Encoder.string selector

  let query_selector_all _selector = failwith "todo"

  let create_text_node text =
    Jx.Obj.call_js Global.document "createTextNode" [| Jx.Encoder.string text |]

  let create_element name =
    Jx.Obj.call_js Global.document "createElement" [| Jx.Encoder.string name |]
end

module Window = struct
  type t = Jx.t

  let this = Global.window
  let to_event_target t = t

  let set_interval f ms =
    Jx.Obj.call_js_unit Global.window "setInterval"
      [| Jx.Encoder.fun1 f; Jx.Encoder.int ms |]

  let set_timeout f ms =
    Jx.Obj.call_js_unit Global.window "setTimeout"
      [| Jx.Encoder.fun1 f; Jx.Encoder.int ms |]

  (* Event handling *)

  let bind event_type f =
    Jx.Obj.call2_unit this "addEventListener" Jx.Encoder.js Jx.Encoder.fun1
      event_type f

  let unbind event_type =
    Jx.Obj.call_js_unit this "removeEventLister" [| Jx.Encoder.js event_type |]
end
