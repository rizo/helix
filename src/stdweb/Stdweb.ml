module Js = Helix_js

module Global = struct
  let this = Js.global_this
  let window = Js.global "window"
  let document = Js.global "document"
  let console = Js.global "console"
end

module Dom = struct
  module Event = struct
    type 'a kind = string

    module Kind = struct
      type 'a t = 'a kind
      type base = unit t

      let to_string t = t
    end

    type 'a t = Js.t
    type target = Js.t

    let target t = Js.Obj.get t "target" Js.Decoder.js

    module Target = struct
      type t = target

      let checked this = Js.Obj.get this "checked" Js.Decoder.bool
      let value this = Js.Obj.get this "value" Js.Decoder.string
      let set_value this value = Js.Obj.set this "value" Js.Encoder.string value
    end

    let target_value ev = Target.value (target ev)

    module Input = struct
      type nonrec kind = [ `Input ] kind
      type t = Js.t

      let data this = Js.Obj.get this "data" Js.Decoder.string
    end

    module Keyboard = struct
      type nonrec kind = [ `Keyboard ] kind
      type t = Js.t

      let key this = Js.Obj.get this "key" Js.Decoder.string
      let code this = Js.Obj.get this "keyCode" Js.Decoder.string
    end

    module Mouse = struct
      type nonrec kind = [ `Mouse ] kind
      type t = Js.t

      let page_x this = Js.Obj.get this "pageX" Js.Decoder.float
      let page_y this = Js.Obj.get this "pageY" Js.Decoder.float
    end

    let click = "click"
    let input = "input"
    let keydown = "keydown"
    let change = "change"
  end

  module Event_target = struct
    type t = Js.t
    type 'a listener = 'a Event.t -> unit

    let add_event_listener this event_name f =
      Js.Obj.call_js_unit this "addEventListener"
        [| Js.Encoder.string event_name; Js.Encoder.fun1 f |]

    let remove_event_listener this event_name =
      Js.Obj.call_js_unit this "removeEventLister"
        [| Js.Encoder.string event_name |]
  end

  module Node = struct
    type t = Js.t
    type node = t

    module List = struct
      type t = Js.t

      let for_each this f =
        Js.Obj.call_js_unit this "forEach" [| Js.Encoder.fun1 f |]
    end

    include (Event_target : module type of Event_target with type t := t)

    let to_js t = t
    let to_event_target t = t
    let parent_node this = Js.Obj.get this "parentNode" Js.Decoder.(nullable js)
    let child_nodes this = Js.Obj.get this "childNodes" Js.Decoder.js
    let first_child this = Js.Obj.get this "firstChild" Js.Decoder.(nullable js)
    let last_child this = Js.Obj.get this "lastChild" Js.Decoder.(nullable js)

    let next_sibling this =
      Js.Obj.get this "nextSibling" Js.Decoder.(nullable js)

    let clone_node this ~deep =
      Js.Obj.call1 this "cloneNode" Js.Encoder.bool deep ~return:Js.Decoder.js

    let append_child ~parent other =
      Js.Obj.call_js_unit parent "appendChild" [| other |]

    let remove_child ~parent other =
      Js.Obj.call_js_unit parent "removeChild" [| other |]

    let insert_before ~parent ~reference new_node =
      Js.Obj.call_js_unit parent "insertBefore" [| new_node; reference |]

    let replace_child ~parent ~reference new_node =
      Js.Obj.call_js_unit parent "replaceChild" [| new_node; reference |]

    let set_text_content this text =
      Js.Obj.set this "textContent" Js.Encoder.string text

    let get_text_content this = Js.Obj.get this "textContent" Js.Decoder.string

    let is_same_node this other =
      Js.Decoder.bool (Js.Obj.call_js this "isSameNode" [| other |])
  end

  module Element = struct
    type t = Js.t

    include (Node : module type of Node with type t := t)

    let to_node t = t

    let replace_children this children =
      Js.Obj.call_js_unit this "replaceChildren" children

    let append this other = Js.Obj.call_js_unit this "append" [| other |]

    let replace_with this other =
      Js.Obj.call_js_unit this "replaceWith" [| other |]

    let set_attribute this name value =
      Js.Obj.call_js_unit this "setAttribute"
        [| Js.Encoder.string name; Js.Encoder.string value |]

    let remove_attribute this name =
      Js.Obj.call_js_unit this "removeAttribute" [| Js.Encoder.string name |]
  end

  module Css_style_declaration = struct
    type t = Js.t

    let css_text this = Js.Obj.get this "cssText" Js.Decoder.string
    let length this = Js.Obj.get this "length" Js.Decoder.int

    let set_property this name value =
      Js.Obj.call_js_unit this "setProperty"
        [| Js.Encoder.string name; Js.Encoder.string value |]

    let get_property this name =
      Js.Decoder.string
        (Js.Obj.call_js this "setProperty" [| Js.Encoder.string name |])

    let remove_property this name =
      Js.Obj.call_js_unit this "removeProperty" [| Js.Encoder.string name |]
  end

  module Html_element = struct
    type t = Js.t

    let of_node t = t
    let of_element t = t
    let to_element t = t
    let get_style this = Js.Obj.get this "style" Js.Decoder.js

    let set_style_property this name value =
      Css_style_declaration.set_property (get_style this) name value

    let get_style_property this name =
      Css_style_declaration.get_property (get_style this) name

    let remove_style_property this name =
      Css_style_declaration.remove_property (get_style this) name
  end

  module Character_data = struct
    type t = Js.t

    let to_node t = t
  end

  module Text = struct
    type t = Js.t

    include (Character_data : module type of Character_data with type t := t)

    let t = Js.global "Text"
    let to_character_data t = t
  end

  module Comment = struct
    type t = Js.t

    include (Character_data : module type of Character_data with type t := t)

    let t = Js.global "Comment"
    let to_character_data t = t
    let make data = Js.Obj.new1 t Js.Encoder.string data
  end

  module Document_fragment = struct
    type t = Js.t

    let t = Js.global "DocumentFragment"
    let to_node t = t
    let make () = Js.Obj.new0 t

    let replace_children this children =
      Js.Obj.call_js_unit this "replaceChildren" children
  end

  module Document = struct
    type t = Js.t

    let this = Global.document
    let to_node this = this

    let get_element_by_id id =
      Js.Decoder.nullable
        (fun x -> x)
        (Js.Obj.call_js Global.document "getElementById"
           [| Js.Encoder.string id |])

    let create_text_node text =
      Js.Obj.call_js Global.document "createTextNode"
        [| Js.Encoder.string text |]

    let create_element name =
      Js.Obj.call_js Global.document "createElement"
        [| Js.Encoder.string name |]
  end

  module Window = struct
    type t = Js.t

    let this = Global.window

    include (Event_target : module type of Event_target with type t := t)

    let to_event_target t = t

    let set_interval f ms =
      Js.Obj.call_js_unit Global.window "setInterval"
        [| Js.Encoder.fun1 f; Js.Encoder.int ms |]

    let set_timeout f ms =
      Js.Obj.call_js_unit Global.window "setTimeout"
        [| Js.Encoder.fun1 f; Js.Encoder.int ms |]
  end
end

module Console = struct
  type t

  let t = Global.console
  let log x = Js.Obj.call_js_unit Global.console "log" [| Js.Encoder.any x |]
end

module Iterator = struct
  type 'a t = Js.t
  type 'a next = Js.t

  let next t = Js.Obj.call_js t "next" [||]
  let next_is_done next = Js.Obj.get next "done" Js.Decoder.bool
  let next_value next = Js.Obj.get next "value" Js.Decoder.any

  let iter f t =
    let is_done = ref false in
    while not !is_done do
      let n = next t in
      if next_is_done n then is_done := true
      else
        let v = next_value n in
        f v
    done
end

module Map = struct
  type 'a t = Js.t

  let t = Js.global "Map"
  let to_js t = t
  let of_js t = t
  let make () = Js.Obj.new0 t
  let clear t = Js.Obj.call_js_unit t "clear" [||]
  let set t k v = Js.Obj.call_js_unit t "set" [| k; Js.Encoder.any v |]
  let get t k = Js.Decoder.any (Js.Obj.call_js t "get" [| k |])
  let delete t k = Js.Obj.call_js_unit t "delete" [| k |]
  let keys t = Js.Obj.call_js t "keys" [||]
  let size t = Js.Obj.get t "size" Js.Decoder.int
  let values t = Js.Obj.call_js t "values" [||]

  let first_key t =
    let iter = keys t in
    let next = Iterator.next iter in
    if Iterator.next_is_done next then None else Some (Iterator.next_value next)
end
