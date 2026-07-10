module Global = Stdweb_global
module Promise = Stdweb_promise

open struct
  module E = Jx.Encode
  module D = Jx.Decode
end

type node = Jx.t
type event = Jx.t

(* Style *)

type style = Jx.t

module Style = struct
  type t = style

  let text this = D.string (Jx.obj_get this "cssText")
  let length this = D.int (Jx.obj_get this "length")

  let set this name value =
    ignore (Jx.obj_call this "setProperty" [| E.string name; E.string value |])

  let unset target name =
    ignore (Jx.obj_call target "removeProperty" [| E.string name |])

  let get this name =
    D.string (Jx.obj_call this "setProperty" [| E.string name |])
end

(* DOM events *)

module Event = struct
  type name = Jx.t

  module Name = struct
    type t = Jx.t

    let to_string = D.string
    let make = E.string
  end

  type t = event

  let target t = D.any (Jx.obj_get t "target")
  let prevent_default t = ignore (Jx.obj_call t "preventDefault" [||])

  (* Generic events *)

  let change = Name.make "change"

  (* UI events *)

  let resize = Name.make "resize"

  (* Mouse *)

  let click = E.string "click"
  let dblclick = E.string "dblclick"
  let mousemove = E.string "mousemove"
  let page_x this = D.float (Jx.obj_get this "pageX")
  let page_y this = D.float (Jx.obj_get this "pageY")

  (* Keyboard events *)

  let keydown = Name.make "keydown"
  let key this = D.string (Jx.obj_get this "key")
  let code this = D.string (Jx.obj_get this "keyCode")

  (* Input *)

  let before_input = Name.make "beforeinput"
  let input = Name.make "input"
  let cancel = Name.make "cancel"
  let data this = D.string (Jx.obj_get this "data")

  (* Fullscreen events *)
  let fullscreen_change = Name.make "fullscreenchange"
  let fullscreen_error = Name.make "fullscreenerror"

  (* Pop state events *)
  let popstate = Name.make "popstate"

  (* Hash change events *)
  let hashchange = Name.make "hashchange"

  (* Focus events *)
  let blur = Name.make "blur"
  let focusin = Name.make "focusin"
  let focusout = Name.make "focusout"

  (* Submit events *)
  let submit = Name.make "submit"
end

module Token_list = struct
  type t = Jx.t

  let toggle this name = ignore (Jx.obj_call this "toggle" [| E.string name |])
  let add this name = ignore (Jx.obj_call this "add" [| E.string name |])
  let remove this name = ignore (Jx.obj_call this "remove" [| E.string name |])
end

module Blob = struct
  type t = Jx.t

  let size t = D.int (Jx.obj_get t "size")
  let type' t = D.string (Jx.obj_get t "type")
  let text t = Jx.obj_call t "text" [||] |> Promise.map D.string
end

module File = struct
  type t = Jx.t

  let last_modified t = D.float (Jx.obj_get t "last_modified")
  let name t = D.string (Jx.obj_get t "name")
  let to_blob t = t
end

module Node = struct
  type t = node

  (* Conversions *)

  let to_js t = t

  (* Traversal *)

  let parent this =
    Stdweb_jx.nullable (fun x -> x) (Jx.obj_get this "parentNode")

  let first_child this =
    Stdweb_jx.nullable (fun x -> x) (Jx.obj_get this "firstChild")

  let last_child this =
    Stdweb_jx.nullable (fun x -> x) (Jx.obj_get this "lastChild")

  let next_sibling this =
    Stdweb_jx.nullable (fun x -> x) (Jx.obj_get this "nextSibling")

  let _next_sibling_js this = Jx.obj_get this "nextSibling"

  let iter_children this f =
    let node_list = Jx.obj_get this "childNodes" in
    ignore (Jx.obj_call node_list "forEach" [| E.func 1 f |])

  let children this =
    let acc = ref [] in
    iter_children this (fun c -> acc := c :: !acc);
    List.rev !acc

  (* Node operations *)

  let clone_node this ~deep = Jx.obj_call this "cloneNode" [| E.bool deep |]

  let is_same_node this other =
    D.bool (Jx.obj_call this "isSameNode" [| other |])

  let remove this = ignore (Jx.obj_call this "remove" [||])

  let replace_with this other =
    ignore (Jx.obj_call this "replaceWith" [| other |])

  (* Children manipulation *)

  let append_text ~parent text =
    ignore (Jx.obj_call parent "append" [| E.string text |])

  let append_child ~parent other =
    ignore (Jx.obj_call parent "appendChild" [| other |])

  let remove_child ~parent other =
    ignore (Jx.obj_call parent "removeChild" [| other |])

  let insert_before ~parent ~reference new_node =
    ignore (Jx.obj_call parent "insertBefore" [| new_node; reference |])

  let insert_after ~parent ~reference new_node =
    insert_before ~parent ~reference:(_next_sibling_js reference) new_node

  let replace_child ~parent ~reference new_node =
    ignore (Jx.obj_call parent "replaceChild" [| new_node; reference |])

  let replace_children this children =
    ignore (Jx.obj_call this "replaceChildren" children)

  let insert_adjacent_element this position node =
    ignore
      (Jx.obj_call this "insertAdjacentElement" [| E.string position; node |])

  let insert_before_begin ~reference node =
    insert_adjacent_element reference "beforebegin" node

  let insert_after_begin ~reference node =
    insert_adjacent_element reference "afterbegin" node

  let insert_before_end ~reference node =
    insert_adjacent_element reference "beforeend" node

  let insert_after_end ~reference node =
    insert_adjacent_element reference "afterend" node

  (* Text content *)

  let set_text_content this text = Jx.obj_set this "textContent" (E.string text)
  let get_text_content this = D.string (Jx.obj_get this "textContent")

  (* HTML content *)

  let set_inner_html this text = Jx.obj_set this "innerHTML" (E.string text)

  (* Event handling *)

  let bind this event_type f =
    ignore (Jx.obj_call this "addEventListener" [| event_type; E.func 1 f |])

  let unbind this event_type f =
    ignore (Jx.obj_call this "removeEventListener" [| event_type; E.func 1 f |])

  (* State changes *)

  let blur this = ignore (Jx.obj_call this "blur" [||])
  let click this = ignore (Jx.obj_call this "click" [||])
  let focus this = ignore (Jx.obj_call this "focus" [||])

  (* Popover operations *)

  let hide_popover this = ignore (Jx.obj_call this "hidePopover" [||])
  let show_popover this = ignore (Jx.obj_call this "showPopover" [||])
  let toggle_popover this = ignore (Jx.obj_call this "togglePopover" [||])

  (* Selectors *)

  let matches this selectors =
    D.bool (Jx.obj_call this "matches" [| E.string selectors |])

  let closest this selectors =
    Stdweb_jx.nullable D.any
      (Jx.obj_call this "closest" [| E.string selectors |])

  (* input *)

  let select this = ignore (Jx.obj_call this "select" [||])

  (* a *)

  let to_string this = D.string (Jx.obj_call this "toString" [||])

  (* Attributes *)

  let set_attr this name value =
    ignore (Jx.obj_call this "setAttribute" [| E.string name; E.string value |])

  let get_attr this name =
    D.string (Jx.obj_call this "getAttribute" [| E.string name |])

  let unset_attr this name =
    ignore (Jx.obj_call this "removeAttribute" [| E.string name |])

  let get_class_list this = Jx.obj_get this "classList"
  let set_style this x = Jx.obj_set this "style" (E.string x)
  let get_style this = Jx.obj_get this "style"
  let reset_style this = Jx.obj_set this "style" (E.string "")
  let get_href this = D.string (Jx.obj_get this "href")
  let set_href this x = Jx.obj_set this "href" (E.string x)
  let get_value this = D.string (Jx.obj_get this "value")
  let set_value this x = Jx.obj_set this "value" (E.string x)
  let reset_value this = Jx.obj_set this "value" (E.string "")
  let set_disabled this x = Jx.obj_set this "disabled" (E.bool x)
  let set_autofocus this x = Jx.obj_set this "autofocus" (E.bool x)
  let get_offset_width this = D.int (Jx.obj_get this "offsetWidth")
  let get_client_width this = D.int (Jx.obj_get this "clientWidth")
  let get_checked this = D.bool (Jx.obj_get this "checked")
  let set_checked this x = Jx.obj_set this "checked" (E.bool x)
  let get_files this = D.array (Jx.obj_get this "files")
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
  let make data = Jx.obj_new t [| E.string data |]
end

module Fragment = struct
  type t = Jx.t

  let t = Jx.global "DocumentFragment"
  let to_node t = t
  let make () = Jx.obj_new t [||]

  let replace_children this children =
    ignore (Jx.obj_call this "replaceChildren" children)
end

module Document = struct
  type t = Jx.t

  let this = Global.document
  let to_node this = this
  let get_cookies () = D.string (Jx.obj_get Global.document "cookie")
  let set_cookies cookies = Jx.obj_set this "cookie" (E.string cookies)

  let get_element_by_id id =
    Stdweb_jx.nullable
      (fun x -> x)
      (Jx.obj_call Global.document "getElementById" [| E.string id |])

  let query_selector selector =
    Stdweb_jx.nullable
      (fun x -> x)
      (Jx.obj_call Global.document "querySelector" [| E.string selector |])

  let query selector =
    Stdweb_jx.nullable
      (fun x -> x)
      (Jx.obj_call Global.document "querySelector" [| E.string selector |])

  let query_selector_all _selector = failwith "todo"

  let create_text_node text =
    Jx.obj_call Global.document "createTextNode" [| E.string text |]

  let create_element name =
    Jx.obj_call Global.document "createElement" [| E.string name |]

  let active_element this = Jx.obj_get this "activeElement"
end

module Location = struct
  type t = Jx.t

  let hash t = D.string (Jx.obj_get t "hash")
  let set_hash t v = Jx.obj_set t "hash" (E.string v)
  let href t = D.string (Jx.obj_get t "href")
  let set_href t v = Jx.obj_set t "href" (E.string v)
  let replace t url = ignore (Jx.obj_call t "replace" [| E.string url |])
  let assign t url = ignore (Jx.obj_call t "assign" [| E.string url |])
  let reload t = D.any (Jx.obj_call t "reload" [||])
  let hostname t = D.string (Jx.obj_get t "hostname")
  let search t = D.string (Jx.obj_get t "search")
end

module Window = struct
  type t = Jx.t

  let this = Global.window
  let to_event_target t = t
  let location = Jx.obj_get this "location"

  let set_interval f ms =
    D.int (Jx.obj_call Global.window "setInterval" [| E.func 0 f; E.int ms |])

  let set_timeout f ms =
    D.int (Jx.obj_call Global.window "setTimeout" [| E.func 0 f; E.int ms |])

  let clear_timeout id =
    ignore (Jx.obj_call Global.window "clearTimeout" [| E.int id |])

  let alert text = ignore (Jx.obj_call this "alert" [| E.string text |])
  let confirm text = D.bool (Jx.obj_call this "confirm" [| E.string text |])

  let prompt text =
    Stdweb_jx.nullable D.string (Jx.obj_call this "prompt" [| E.string text |])

  (* Event handling *)

  let bind event_type f =
    ignore (Jx.obj_call this "addEventListener" [| event_type; E.func 1 f |])

  let unbind event_type =
    ignore (Jx.obj_call this "removeEventLister" [| event_type |])
end
