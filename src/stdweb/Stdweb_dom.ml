module Global = Stdweb_global

module Event = struct
  type 'a kind = string

  module Kind = struct
    type 'a t = 'a kind
    type base = unit t

    let to_string t = t
  end

  type 'a t = Jx.t
  type target = Jx.t

  let target t = Jx.Obj.get t "target" Jx.Decoder.js

  module Target = struct
    type t = target

    let checked this = Jx.Obj.get this "checked" Jx.Decoder.bool
    let value this = Jx.Obj.get this "value" Jx.Decoder.string
    let set_value this value = Jx.Obj.set this "value" Jx.Encoder.string value
  end

  let target_value ev = Target.value (target ev)

  module Input = struct
    type nonrec kind = [ `Input ] kind
    type t = Jx.t

    let data this = Jx.Obj.get this "data" Jx.Decoder.string
  end

  module Keyboard = struct
    type nonrec kind = [ `Keyboard ] kind
    type t = Jx.t

    let key this = Jx.Obj.get this "key" Jx.Decoder.string
    let code this = Jx.Obj.get this "keyCode" Jx.Decoder.string
  end

  module Mouse = struct
    type nonrec kind = [ `Mouse ] kind
    type t = Jx.t

    let page_x this = Jx.Obj.get this "pageX" Jx.Decoder.float
    let page_y this = Jx.Obj.get this "pageY" Jx.Decoder.float
  end

  let click = "click"
  let input = "input"
  let keydown = "keydown"
  let change = "change"
end

module Event_target = struct
  type t = Jx.t
  type 'a listener = 'a Event.t -> unit

  let add_event_listener this event_name f =
    Jx.Obj.call_js_unit this "addEventListener"
      [| Jx.Encoder.string event_name; Jx.Encoder.fun1 f |]

  let remove_event_listener this event_name =
    Jx.Obj.call_js_unit this "removeEventLister"
      [| Jx.Encoder.string event_name |]
end

module Node = struct
  type t = Jx.t
  type node = t

  module List = struct
    type t = Jx.t

    let for_each this f =
      Jx.Obj.call_js_unit this "forEach" [| Jx.Encoder.fun1 f |]
  end

  include (Event_target : module type of Event_target with type t := t)

  let to_js t = t
  let to_event_target t = t
  let parent_node this = Jx.Obj.get this "parentNode" Jx.Decoder.(nullable js)
  let child_nodes this = Jx.Obj.get this "childNodes" Jx.Decoder.js
  let first_child this = Jx.Obj.get this "firstChild" Jx.Decoder.(nullable js)
  let last_child this = Jx.Obj.get this "lastChild" Jx.Decoder.(nullable js)
  let next_sibling this = Jx.Obj.get this "nextSibling" Jx.Decoder.(nullable js)

  let clone_node this ~deep =
    Jx.Obj.call1 this "cloneNode" Jx.Encoder.bool deep ~return:Jx.Decoder.js

  let append_child ~parent other =
    Jx.Obj.call_js_unit parent "appendChild" [| other |]

  let remove_child ~parent other =
    Jx.Obj.call_js_unit parent "removeChild" [| other |]

  let insert_before ~parent ~reference new_node =
    Jx.Obj.call_js_unit parent "insertBefore" [| new_node; reference |]

  let replace_child ~parent ~reference new_node =
    Jx.Obj.call_js_unit parent "replaceChild" [| new_node; reference |]

  let set_text_content this text =
    Jx.Obj.set this "textContent" Jx.Encoder.string text

  let get_text_content this = Jx.Obj.get this "textContent" Jx.Decoder.string

  let is_same_node this other =
    Jx.Decoder.bool (Jx.Obj.call_js this "isSameNode" [| other |])
end

module Token_list = struct
  type t = Jx.t

  let toggle_class class_list name =
    Jx.Obj.call1_unit class_list "toggle" Jx.Encoder.string name
end

module Element = struct
  type t = Jx.t

  include (Node : module type of Node with type t := t)

  let to_node t = t

  let replace_children this children =
    Jx.Obj.call_js_unit this "replaceChildren" children

  let append this other = Jx.Obj.call_js_unit this "append" [| other |]

  let replace_with this other =
    Jx.Obj.call_js_unit this "replaceWith" [| other |]

  let set_attribute this name value =
    Jx.Obj.call_js_unit this "setAttribute"
      [| Jx.Encoder.string name; Jx.Encoder.string value |]

  let remove_attribute this name =
    Jx.Obj.call_js_unit this "removeAttribute" [| Jx.Encoder.string name |]

  let class_list this = Jx.Obj.get this "classList" Jx.Decoder.any
  let of_target t = t

  let matches this selectors =
    Jx.Decoder.bool
      (Jx.Obj.call_js this "matches" [| Jx.Encoder.string selectors |])

  let closest this selectors =
    Jx.Decoder.nullable Jx.Decoder.any
      (Jx.Obj.call_js this "closest" [| Jx.Encoder.string selectors |])
end

module Css_style_declaration = struct
  type t = Jx.t

  let css_text this = Jx.Obj.get this "cssText" Jx.Decoder.string
  let length this = Jx.Obj.get this "length" Jx.Decoder.int

  let set_property this name value =
    Jx.Obj.call_js_unit this "setProperty"
      [| Jx.Encoder.string name; Jx.Encoder.string value |]

  let get_property this name =
    Jx.Decoder.string
      (Jx.Obj.call_js this "setProperty" [| Jx.Encoder.string name |])

  let remove_property target name =
    Jx.Obj.call_js_unit target "removeProperty" [| Jx.Encoder.string name |]
end

module Html_element = struct
  type t = Jx.t

  let of_node t = t
  let of_element t = t
  let to_element t = t
  let get_style this = Jx.Obj.get this "style" Jx.Decoder.js

  let set_style_property this name value =
    Css_style_declaration.set_property (get_style this) name value

  let get_style_property this name =
    Css_style_declaration.get_property (get_style this) name

  let remove_style_property this name =
    Css_style_declaration.remove_property (get_style this) name
end

module Character_data = struct
  type t = Jx.t

  let to_node t = t
end

module Text = struct
  type t = Jx.t

  include (Character_data : module type of Character_data with type t := t)

  let t = Jx.global "Text"
  let to_character_data t = t
end

module Comment = struct
  type t = Jx.t

  include (Character_data : module type of Character_data with type t := t)

  let t = Jx.global "Comment"
  let to_character_data t = t
  let make data = Jx.Obj.new1 t Jx.Encoder.string data
end

module Document_fragment = struct
  type t = Jx.t

  let t = Jx.global "DocumentFragment"
  let to_node t = t
  let make () = Jx.Obj.new0 t

  let replace_children this children =
    Jx.Obj.call_js_unit this "replaceChildren" children
end

module Document = struct
  type t = Jx.t
  type 'a listener = 'a Event.t -> unit

  let this = Global.document
  let to_node this = this
  let get_cookies () = Jx.Obj.get Global.document "cookie" Jx.Decoder.string
  let set_cookies cookies = Jx.Obj.set this "cookie" Jx.Encoder.string cookies

  let add_event_listener this event_name f =
    Jx.Obj.call_js_unit this "addEventListener"
      [| Jx.Encoder.string event_name; Jx.Encoder.fun1 f |]

  let get_element_by_id id =
    Jx.Decoder.nullable
      (fun x -> x)
      (Jx.Obj.call_js Global.document "getElementById"
         [| Jx.Encoder.string id |]
      )

  let query_selector selector =
    Jx.Decoder.nullable
      (fun x -> x)
      (Jx.Obj.call_js Global.document "querySelector"
         [| Jx.Encoder.string selector |]
      )

  let create_text_node text =
    Jx.Obj.call_js Global.document "createTextNode" [| Jx.Encoder.string text |]

  let create_element name =
    Jx.Obj.call_js Global.document "createElement" [| Jx.Encoder.string name |]
end

module Window = struct
  type t = Jx.t

  let this = Global.window

  include (Event_target : module type of Event_target with type t := t)

  let to_event_target t = t

  let set_interval f ms =
    Jx.Obj.call_js_unit Global.window "setInterval"
      [| Jx.Encoder.fun1 f; Jx.Encoder.int ms |]

  let set_timeout f ms =
    Jx.Obj.call_js_unit Global.window "setTimeout"
      [| Jx.Encoder.fun1 f; Jx.Encoder.int ms |]
end
