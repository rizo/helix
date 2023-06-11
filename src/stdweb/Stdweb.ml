module Stdlib_obj = Obj
module Stdlib_array = Array

module Global = struct
  let this = Jx.global_this
  let window = Jx.global "window"
  let document = Jx.global "document"
  let console = Jx.global "console"
end

module Dict = struct
  type 'a t = Jx.Obj.t

  let to_js p = p
  let of_js js = js
  let to_obj p = p
  let of_obj obj = obj
  let empty = Jx.Obj.empty

  let of_array arr =
    let arr =
      (Stdlib_obj.magic : (string * 'a) array -> (string * Jx.t) array) arr
    in
    Jx.Obj.of_array arr

  let of_list l = of_array (Stdlib_array.of_list l)
  let get dict key = Jx.Obj.get dict key Jx.Decoder.any
  let get_opt dict key = Jx.Obj.get_opt dict key Jx.Decoder.any
  let set dict key x = Jx.Obj.set dict key Jx.Encoder.any x
  let del = Jx.Obj.del

  let entry_of_js entry_js =
    match Jx.Decoder.array_js entry_js with
    | [| key; v |] -> (Jx.Decoder.string key, Jx.Decoder.any v)
    | _ -> invalid_arg "Object entries is not a pair"

  let entries dict =
    Jx.Obj.call1 Jx.Obj.t "entries"
      ~return:(Jx.Decoder.array entry_of_js)
      Jx.Encoder.js dict

  let keys dict =
    Jx.Obj.call1 Jx.Obj.t "keys"
      ~return:Jx.Decoder.(array string)
      Jx.Encoder.js dict

  let values dict =
    Jx.Obj.call1 Jx.Obj.t "values"
      ~return:Jx.Decoder.(array any)
      Jx.Encoder.js dict

  let map dict f =
    let out = empty () in
    let keys = keys dict in
    for i = 0 to Stdlib_array.length keys - 1 do
      let key = Stdlib_array.unsafe_get keys i in
      let x = get dict key in
      let x' = f x in
      set out key x'
    done;
    out

  let update dict f =
    let keys = keys dict in
    for i = 0 to Stdlib_array.length keys - 1 do
      let key = Stdlib_array.unsafe_get keys i in
      let x = get dict key in
      let x' = f x in
      set dict key x'
    done

  let fold_left dict f init =
    let acc = ref init in
    let values = values dict in
    for i = 0 to Stdlib_array.length values - 1 do
      let x = Stdlib_array.unsafe_get values i in
      acc := f !acc x
    done;
    !acc

  let iter dict f =
    let values = values dict in
    for i = 0 to Stdlib_array.length values - 1 do
      let x = Stdlib_array.unsafe_get values i in
      f x
    done
end

module Array = struct
  type 'a t = Jx.t

  let t = Jx.global "Array"
  let to_js p = p
  let of_js js = js
  let make n = Jx.Obj.new1 t Jx.Encoder.int n
  let empty () = make 0

  let set arr (i : int) x =
    Jx.Obj.set_js arr (Jx.Encoder.int i) (Jx.Encoder.any x)

  let init n f =
    let out = make n in
    for i = 0 to n - 1 do
      set out i (f i)
    done;
    out

  let get arr i = Jx.Decoder.any (Jx.Obj.get_js arr (Jx.Encoder.int i))

  let get_opt arr i =
    let x = get arr i in
    let x_js = Jx.Encoder.any x in
    if Jx.is_undefined x_js then raise Not_found else x

  let push arr x = Jx.Obj.call1_unit arr "push" Jx.Encoder.any x
  let pop arr = Jx.Obj.call0 arr "pop" ~return:Jx.Decoder.any ()
  let pop_opt arr = Jx.Obj.call0 arr "pop" ~return:Jx.Decoder.(optional any) ()
  let length arr = Jx.Obj.get arr "length" Jx.Decoder.int
  let iter arr f = Jx.Obj.call1_unit arr "forEach" Jx.Encoder.fun1 f

  let of_list l =
    match l with
    | [] -> empty ()
    | hd :: tl ->
      let out = make 1 in
      set out 0 hd;
      List.iteri (fun i x -> set out (i + 1) x) tl;
      out
end

module Promise = struct
  type 'a t = Jx.t
  type ('a, 'err) executor = ('a -> unit) -> ('err -> unit) -> unit

  let to_js p = p
  let of_js js = js
  let t = Jx.global "Promise"
  let make executor = Jx.Obj.new1 t Jx.Encoder.fun1 executor
  let resolve v = Jx.Obj.call1 t "resolve" ~return:of_js Jx.Encoder.any v
  let reject err = Jx.Obj.call1 t "reject" ~return:of_js Jx.Encoder.any err
  let and_then f p = Jx.Obj.call1 p "then" ~return:of_js Jx.Encoder.fun1 f
  let use f p = Jx.Obj.call1_unit p "then" Jx.Encoder.fun1 f
end

module Dom = struct
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

    let next_sibling this =
      Jx.Obj.get this "nextSibling" Jx.Decoder.(nullable js)

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

    let remove_property this name =
      Jx.Obj.call_js_unit this "removeProperty" [| Jx.Encoder.string name |]
  end

  module Html_element = struct
    type t = Jx.t

    let t = Jx.global "HTMLElement"
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

    let this = Global.document
    let to_node this = this

    let get_element_by_id id =
      Jx.Decoder.nullable
        (fun x -> x)
        (Jx.Obj.call_js Global.document "getElementById"
           [| Jx.Encoder.string id |]
        )

    let create_text_node text =
      Jx.Obj.call_js Global.document "createTextNode"
        [| Jx.Encoder.string text |]

    let create_element name =
      Jx.Obj.call_js Global.document "createElement"
        [| Jx.Encoder.string name |]
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

  module Custom_element_registry = struct
    type t = Jx.t

    let t = Jx.global "CustomElementRegistry"

    let define name constr =
      Jx.Obj.call2_unit t "define" Jx.Encoder.string Jx.Encoder.any name constr
  end
end

module Console = struct
  let t = Global.console
  let log x = Jx.Obj.call1_unit t "log" Jx.Encoder.any x
  let error x = Jx.Obj.call1_unit t "error" Jx.Encoder.any x
  let info x = Jx.Obj.call1_unit t "info" Jx.Encoder.any x
  let warn x = Jx.Obj.call1_unit t "warn" Jx.Encoder.any x

  let ensure b x =
    Jx.Obj.call2_unit t "assert" Jx.Encoder.bool Jx.Encoder.any b x
end

module Iterator = struct
  type 'a t = Jx.t
  type 'a next = Jx.t

  let next t = Jx.Obj.call_js t "next" [||]
  let next_is_done next = Jx.Obj.get next "done" Jx.Decoder.bool
  let next_value next = Jx.Obj.get next "value" Jx.Decoder.any

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
  type 'a t = Jx.t

  let t = Jx.global "Map"
  let to_js t = t
  let of_js t = t
  let make () = Jx.Obj.new0 t
  let clear t = Jx.Obj.call_js_unit t "clear" [||]
  let set t k v = Jx.Obj.call_js_unit t "set" [| k; Jx.Encoder.any v |]
  let get t k = Jx.Decoder.any (Jx.Obj.call_js t "get" [| k |])
  let delete t k = Jx.Obj.call_js_unit t "delete" [| k |]
  let keys t = Jx.Obj.call_js t "keys" [||]
  let size t = Jx.Obj.get t "size" Jx.Decoder.int
  let values t = Jx.Obj.call_js t "values" [||]

  let first_key t =
    let iter = keys t in
    let next = Iterator.next iter in
    if Iterator.next_is_done next then None else Some (Iterator.next_value next)
end
