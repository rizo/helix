open Metajs

module Dom = struct
  module Event = struct
    type 'a kind = string

    module Kind = struct
      type 'a t = 'a kind
      type base = unit t

      let to_string t = t
    end

    type 'a t = js
    type target = js

    let target (t : 'a t) : target = get_prop t "target"

    module Target = struct
      type t = target

      let checked : t -> bool = fun this -> bool_of_js (get_prop this "checked")
      let value : t -> string = fun this -> string_of_js (get_prop this "value")

      let set_value : t -> string -> unit =
       fun this value -> set_prop this "value" (js_of_string value)
    end

    let target_value ev = Target.value (target ev)

    module Input = struct
      type nonrec kind = [ `Input ] kind
      type t = js

      let data this = string_of_js (get_prop this "data")
    end

    module Keyboard = struct
      type nonrec kind = [ `Keyboard ] kind
      type t = js

      let key this = string_of_js (get_prop this "key")
      let code this = string_of_js (get_prop this "keyCode")
    end

    module Mouse = struct
      type nonrec kind = [ `Mouse ] kind
      type t = js

      let page_x this = float_of_js (get_prop this "pageX")
      let page_y this = float_of_js (get_prop this "pageY")
    end

    let click = "click"
    let input = "input"
    let keydown = "keydown"
    let change = "change"
  end

  module Event_target = struct
    type t = js
    type 'a listener = 'a Event.t -> unit

    let add_event_listener : t -> string -> ('a Event.t -> unit) -> unit =
     fun this event_name f ->
      meth_call_unit this "addEventListener"
        [| js_of_string event_name; callback ~arity:1 f |]

    let remove_event_listener : t -> string -> unit =
     fun this event_name ->
      meth_call_unit this "removeEventLister" [| js_of_string event_name |]
  end

  module Node = struct
    type t = js
    type node = t

    module List = struct
      type t = js

      let for_each : t -> (node -> unit) -> unit =
       fun this f -> meth_call_unit this "forEach" [| callback ~arity:1 f |]
    end

    include (Event_target : module type of Event_target with type t := t)

    let as_js t = t
    let as_event_target t = t

    let parent_node : t -> node option =
     fun this -> option_of_js (fun x -> x) (get_prop this "parentNode")

    let child_nodes : t -> List.t = fun this -> get_prop this "childNodes"

    let first_child : t -> node option =
     fun this -> option_of_js (fun x -> x) (get_prop this "firstChild")

    let last_child : t -> node option =
     fun this -> option_of_js (fun x -> x) (get_prop this "lastChild")

    let next_sibling : t -> node option =
     fun this -> option_of_js (fun x -> x) (get_prop this "nextSibling")

    let clone_node this ~deep = meth_call this "cloneNode" [| js_of_bool deep |]

    let append_child : parent:t -> t -> unit =
     fun ~parent other -> meth_call_unit parent "appendChild" [| other |]

    let remove_child : parent:t -> t -> unit =
     fun ~parent other -> meth_call_unit parent "removeChild" [| other |]

    let insert_before : parent:t -> reference:t -> t -> unit =
     fun ~parent ~reference new_node ->
      meth_call_unit parent "insertBefore" [| new_node; reference |]

    let replace_child : parent:t -> reference:t -> t -> unit =
     fun ~parent ~reference new_node ->
      meth_call_unit parent "replaceChild" [| new_node; reference |]

    let set_text_content : t -> string -> unit =
     fun this text -> set_prop this "textContent" (js_of_string text)

    let get_text_content : t -> string =
     fun this -> string_of_js (get_prop this "textContent")

    let is_same_node : t -> t -> bool =
     fun this other -> bool_of_js (meth_call this "isSameNode" [| other |])
  end

  module Element = struct
    type t = js

    include (Node : module type of Node with type t := t)

    let as_node t = t

    let replace_children : t -> t array -> unit =
     fun this children -> meth_call_unit this "replaceChildren" children

    let append : t -> t -> unit =
     fun this other -> meth_call_unit this "append" [| other |]

    let replace_with : t -> t -> unit =
     fun this other -> meth_call_unit this "replaceWith" [| other |]

    let set_attribute : t -> string -> string -> unit =
     fun this name value ->
      meth_call_unit this "setAttribute"
        [| js_of_string name; js_of_string value |]

    let remove_attribute : t -> string -> unit =
     fun this name ->
      meth_call_unit this "removeAttribute" [| js_of_string name |]
  end

  module Css_style_declaration = struct
    type t = js

    let css_text : t -> string =
     fun this -> string_of_js (get_prop this "cssText")

    let length : t -> int = fun this -> int_of_js (get_prop this "length")

    let set_property : t -> string -> string -> unit =
     fun this name value ->
      meth_call_unit this "setProperty"
        [| js_of_string name; js_of_string value |]

    let get_property : t -> string -> string =
     fun this name ->
      string_of_js (meth_call this "setProperty" [| js_of_string name |])

    let remove_property : t -> string -> unit =
     fun this name ->
      meth_call_unit this "removeProperty" [| js_of_string name |]
  end

  module Html_element = struct
    type t = js

    let of_node t = t
    let of_element t = t
    let as_element t = t

    let get_style : t -> Css_style_declaration.t =
     fun this -> get_prop this "style"

    let set_style_property : t -> string -> string -> unit =
     fun this name value ->
      Css_style_declaration.set_property (get_style this) name value

    let get_style_property : t -> string -> string =
     fun this name -> Css_style_declaration.get_property (get_style this) name

    let remove_style_property : t -> string -> unit =
     fun this name ->
      Css_style_declaration.remove_property (get_style this) name
  end

  module Character_data = struct
    type t = js

    let as_node t = t
  end

  module Text = struct
    type t = js

    include (Character_data : module type of Character_data with type t := t)

    let t = get_prop global "Text"
    let as_character_data t = t
  end

  module Comment = struct
    type t = js

    include (Character_data : module type of Character_data with type t := t)

    let t = get_prop global "Comment"
    let as_character_data t = t
    let make : string -> t = fun data -> new_obj t [| js_of_string data |]
  end

  module Document_fragment = struct
    type t = js

    let t = get_prop global "DocumentFragment"
    let as_node t = t
    let make : unit -> t = fun () -> new_obj t [||]

    let replace_children : t -> t array -> unit =
     fun this children -> meth_call_unit this "replaceChildren" children
  end

  module Document = struct
    type t = Metajs.js

    let this = Global.document
    let as_node this = this

    let get_element_by_id : string -> Element.t option =
     fun id ->
      option_of_js
        (fun x -> x)
        (meth_call Global.document "getElementById" [| js_of_string id |])

    let create_text_node : string -> Text.t =
     fun text ->
      meth_call Global.document "createTextNode" [| js_of_string text |]

    let create_element name =
      meth_call Global.document "createElement" [| js_of_string name |]
  end

  module Window = struct
    type t = js

    let this = Global.window

    include (Event_target : module type of Event_target with type t := t)

    let as_event_target t = t

    let set_interval : (unit -> unit) -> int -> unit =
     fun f ms ->
      meth_call_unit Global.window "setInterval"
        [| callback ~arity:1 f; js_of_int ms |]

    let set_timeout : (unit -> unit) -> int -> unit =
     fun f ms ->
      meth_call_unit Global.window "setTimeout"
        [| callback ~arity:1 f; js_of_int ms |]
  end
end

module Console = struct
  let log : 'a -> unit =
   fun x -> meth_call_unit Global.console "log" [| Metajs.repr x |]
end

module Object = struct
  type t = Metajs.js

  let t = get_prop global "Object"

  let entry_of_js js =
    match Metajs.array_of_js js with
    | [| key; v |] -> (Metajs.string_of_js key, v)
    | _ -> invalid_arg "Object entries is not a pair"

  let entries obj =
    Array.map entry_of_js (Metajs.array_of_js (meth_call t "entries" [| obj |]))
end

module Iterator = struct
  open Metajs

  type t = js
  type next = js

  let t = get_prop global "Map"
  let next t = meth_call t "next" [||]
  let next_is_done next = bool_of_js (get_prop next "done")
  let next_value next = get_prop next "value"

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
  open Metajs

  type t = js

  let t = get_prop global "Map"
  let to_js t = t
  let of_js t = t
  let make () = new_obj t [||]
  let clear t = meth_call_unit t "clear" [||]
  let set t k v = meth_call_unit t "set" [| k; v |]
  let get t k = meth_call t "get" [| k |]
  let delete t k = meth_call_unit t "delete" [| k |]
  let keys t = meth_call t "keys" [||]
  let size t = int_of_js (get_prop t "size")
  let values t = meth_call t "values" [||]
end
