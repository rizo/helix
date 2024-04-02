open Stdweb

(* Attributes are functions that set "attributes" on elements.
   Html values are functions that mount and unmount children. *)

type attr = { set : Dom.Node.t -> unit; unset : Dom.Node.t -> unit }

type html = {
  mount : parent:Dom.node -> insert:(Dom.node -> unit) -> unit;
  unmount : unit -> unit;
}

module Attr = struct
  type t = attr

  let empty = { set = (fun _ -> ()); unset = (fun _ -> ()) }
  let on bool attr = if bool then attr else empty

  let on_some = function
    | None -> empty
    | Some at -> at

  let on_ok = function
    | Error _ -> empty
    | Ok attr -> attr

  let string name value =
    {
      set = (fun el -> Dom.Node.set_attr el name value);
      unset = (fun el -> Dom.Node.unset_attr el name);
    }

  let bool name bool = if bool then string name "" else empty
  let int name i = string name (string_of_int i)
  let on_mount f = { set = (fun node -> f node); unset = (fun _ -> ()) }
  let set attr node = attr.set node
  let unset attr node = attr.unset node
  let make ~set ~unset () = { set; unset }

  let combine a1 a2 =
    {
      set =
        (fun node ->
          a1.set node;
          a2.set node
        );
      unset =
        (fun node ->
          a1.unset node;
          a2.unset node
        );
    }

  let list attrs = List.fold_left combine empty attrs
  let label = string "label"
end

let attr = Attr.string

(* Standard constructors. *)
let accept value = Attr.string "accept" value
let accesskey value = Attr.string "accesskey" value
let action value = Attr.string "action" value
let autocomplete value = Attr.string "autocomplete" value
let autofocus value = Attr.bool "autofocus" value
let charset value = Attr.string "charset" value
let checked value = Attr.bool "checked" value
let class_name value = Attr.string "class" value
let cols value = Attr.int "cols" value
let content value = Attr.string "content" value
let contenteditable value = Attr.bool "contenteditable" value
let defer value = Attr.bool "defer" value
let dir value = Attr.string "dir" value
let disabled value = Attr.bool "disabled" value
let draggable value = Attr.bool "draggable" value
let for' value = Attr.string "for" value
let formaction value = Attr.string "formaction" value
let height value = Attr.int "height" value
let hidden value = Attr.bool "hidden" value
let href value = Attr.string "href" value
let id value = Attr.string "id" value
let lang value = Attr.string "lang" value
let list value = Attr.string "list" value
let media value = Attr.string "media" value
let method' value = Attr.string "method" value
let name value = Attr.string "name" value
let open' value = Attr.bool "open" value
let placeholder value = Attr.string "placeholder" value
let rel value = Attr.string "rel" value
let required value = Attr.bool "required" value
let rows value = Attr.int "rows" value
let selected value = Attr.bool "selected" value
let spellcheck value = Attr.string "spellcheck" value
let src value = Attr.string "src" value
let tabindex value = Attr.int "tabindex" value
let title value = Attr.string "title" value
let type' value = Attr.string "type" value

let value x =
  { set = (fun el -> Dom.Node.set_value el x); unset = Dom.Node.reset_value }

let value_or default opt =
  match opt with
  | Some x -> value x
  | None -> value default

let wrap value = Attr.string "wrap" value
let width value = Attr.int "width" value
let style x = Attr.string "style" x
let role x = Attr.string "role" x

let style_list items =
  let set node =
    let style = Dom.Node.get_style node in
    List.iter (fun (name, value) -> Dom.Style.set style name value) items
  in
  let unset node =
    let style = Dom.Node.get_style node in
    List.iter (fun (name, _value) -> Dom.Style.unset style name) items
  in
  { set; unset }

let class_list items =
  let set node =
    let cl = Dom.Node.get_class_list node in
    List.iter (fun name -> Dom.Token_list.add cl name) items
  in
  let unset node =
    let cl = Dom.Node.get_class_list node in
    List.iter (fun name -> Dom.Token_list.remove cl name) items
  in
  { set; unset }

let class_flags options =
  let list =
    List.fold_left (fun acc (c, b) -> if b then c :: acc else acc) [] options
  in
  class_list list

let on ?(default = true) ?confirm (name : Dom.Event.name) f =
  let f' =
    match confirm with
    | None when not default ->
      fun ev ->
        Dom.Event.prevent_default ev;
        f ev
    | None -> f
    | Some msg ->
      fun ev ->
        if Dom.Window.confirm msg then
          let () = if not default then Dom.Event.prevent_default ev in
          f ev
        else Dom.Event.prevent_default ev
  in
  {
    set = (fun el -> Dom.Node.bind el name f');
    unset = (fun el -> Dom.Node.unbind el name f');
  }

let on_change ?confirm handler =
  on ~default:false ?confirm Dom.Event.change (fun ev ->
      handler (Dom.Node.get_value (Dom.Event.target ev))
  )

let on_checked ?confirm handler =
  on ~default:false ?confirm Dom.Event.change (fun ev ->
      handler (Dom.Node.get_checked (Dom.Event.target ev))
  )

let on_input ?confirm handler =
  on ~default:false ?confirm Dom.Event.input (fun ev ->
      handler (Dom.Node.get_value (Dom.Event.target ev))
  )

let on_click ?confirm handler =
  on ~default:false ?confirm Dom.Event.click (fun _ -> handler ())

let on_double_click ?confirm handler =
  on ~default:false ?confirm Dom.Event.dblclick (fun _ -> handler ())

(* Elem *)

module Elem_util = struct
  let mount ~parent ?(insert = Dom.Node.append_child ~parent) html =
    html.mount ~parent ~insert

  let unmount html = html.unmount ()
end

let elem name attrs children =
  let node_ref = ref None in
  let mount ~parent:_ ~insert =
    let node = Dom.Document.create_element name in
    node_ref := Some node;
    insert node;
    List.iter
      (fun (child : html) -> Elem_util.mount ~parent:node child)
      children;
    List.iter (fun attr -> Attr.set attr node) attrs
  in
  let unmount () =
    match !node_ref with
    | None ->
      Jx.log (Jx.Encoder.string ("#ELEM: " ^ name));
      failwith "bug: attempting to remove an element that was not mounted."
    | Some node -> (
      (* FIXME: this is likely an overkill (but ensures correctness).
         Ideally we do not want to unset regular attrs for nodes that will be removed.
         We only need this for stateful attrs (like signal "bind" events). *)
      List.iter (fun attr -> Attr.unset attr node) attrs;
      List.iter (fun (child : html) -> child.unmount ()) children;
      match Dom.Node.parent node with
      | Some parent -> Dom.Node.remove_child ~parent node
      | None ->
        Jx.log (Jx.Encoder.string ("#ELEM: " ^ name));
        failwith "bug: attempting to remove an element without a parent."
    )
  in
  { mount; unmount }

let fragment children =
  let mount ~parent:_ ~insert =
    let fragment_node = Dom.Fragment.make () in
    List.iter
      (fun child_html -> Elem_util.mount ~parent:fragment_node child_html)
      children;
    insert fragment_node
  in
  let unmount () = List.iter Elem_util.unmount children in
  { mount; unmount }

let text data =
  let node_ref = ref None in
  let mount ~parent:_ ~insert =
    let node = Dom.Document.create_text_node data in
    insert node;
    node_ref := Some node
  in
  let unmount () =
    match !node_ref with
    | None ->
      failwith
        ("bug: attempting to remove a text node that was not mounted: " ^ data)
    | Some node -> (
      match Dom.Node.parent node with
      | Some parent -> Dom.Node.remove_child ~parent node
      | None ->
        Jx.log (Jx.Encoder.string ("#TEXT: " ^ data));
        failwith
          ("bug: attempting to remove a text node without a parent: " ^ data)
    )
  in
  { mount; unmount }

let int n = text (string_of_int n)

let empty =
  { mount = (fun ~parent:_ ~insert:_ -> ()); unmount = (fun () -> ()) }

let nbsp = text "\u{00A0}"
let a attrs children = elem "a" attrs children
let abbr attrs children = elem "abbr" attrs children
let address attrs children = elem "address" attrs children
let area attrs = elem "area" attrs []
let article attrs children = elem "article" attrs children
let aside attrs children = elem "aside" attrs children
let audio attrs children = elem "audio" attrs children
let b attrs children = elem "b" attrs children
let base attrs = elem "base" attrs []
let bdi attrs children = elem "bdi" attrs children
let bdo attrs children = elem "bdo" attrs children
let blockquote attrs children = elem "blockquote" attrs children
let br attrs = elem "br" attrs []
let button attrs children = elem "button" attrs children
let canvas attrs children = elem "canvas" attrs children
let caption attrs children = elem "caption" attrs children
let cite attrs children = elem "cite" attrs children
let code attrs children = elem "code" attrs children
let col attrs = elem "col" attrs []
let colgroup attrs children = elem "colgroup" attrs children
let command attrs children = elem "command" attrs children
let datalist attrs children = elem "datalist" attrs children
let dd attrs children = elem "dd" attrs children
let del attrs children = elem "del" attrs children
let details attrs children = elem "details" attrs children
let dfn attrs children = elem "dfn" attrs children
let div attrs children = elem "div" attrs children
let dl attrs children = elem "dl" attrs children
let dt attrs children = elem "dt" attrs children
let em attrs children = elem "em" attrs children
let embed attrs = elem "embed" attrs []
let fieldset attrs children = elem "fieldset" attrs children
let figcaption attrs children = elem "figcaption" attrs children
let figure attrs children = elem "figure" attrs children
let footer attrs children = elem "footer" attrs children
let form attrs children = elem "form" attrs children
let h1 attrs children = elem "h1" attrs children
let h2 attrs children = elem "h2" attrs children
let h3 attrs children = elem "h3" attrs children
let h4 attrs children = elem "h4" attrs children
let h5 attrs children = elem "h5" attrs children
let h6 attrs children = elem "h6" attrs children
let head attrs children = elem "head" attrs children
let header attrs children = elem "header" attrs children
let hgroup attrs children = elem "hgroup" attrs children
let hr attrs = elem "hr" attrs []

(* let html attrs children = elem "html" attrs children *)
let i attrs children = elem "i" attrs children
let iframe attrs children = elem "iframe" attrs children
let img attrs = elem "img" attrs []
let input attrs = elem "input" attrs []
let ins attrs children = elem "ins" attrs children
let kbd attrs children = elem "kbd" attrs children
let keygen attrs children = elem "keygen" attrs children
let label attrs children = elem "label" attrs children
let legend attrs children = elem "legend" attrs children
let li attrs children = elem "li" attrs children
let main attrs children = elem "main" attrs children
let map attrs children = elem "map" attrs children
let mark attrs children = elem "mark" attrs children
let menu attrs children = elem "menu" attrs children
let meta attrs = elem "meta" attrs []
let meter attrs children = elem "meter" attrs children
let nav attrs children = elem "nav" attrs children
let object' attrs children = elem "object" attrs children
let ol attrs children = elem "ol" attrs children
let optgroup attrs children = elem "optgroup" attrs children
let option attrs children = elem "option" attrs children
let output attrs children = elem "output" attrs children
let p attrs children = elem "p" attrs children
let param attrs = elem "param" attrs []
let pre attrs children = elem "pre" attrs children
let progress attrs children = elem "progress" attrs children
let q attrs children = elem "q" attrs children
let rp attrs children = elem "rp" attrs children
let rt attrs children = elem "rt" attrs children
let ruby attrs children = elem "ruby" attrs children
let s attrs children = elem "s" attrs children
let samp attrs children = elem "samp" attrs children
let section attrs children = elem "section" attrs children
let select attrs children = elem "select" attrs children
let small attrs children = elem "small" attrs children
let source attrs = elem "source" attrs []
let span attrs children = elem "span" attrs children
let strong attrs children = elem "strong" attrs children
let sub attrs children = elem "sub" attrs children
let summary attrs children = elem "summary" attrs children
let sup attrs children = elem "sup" attrs children
let table attrs children = elem "table" attrs children
let tbody attrs children = elem "tbody" attrs children
let td attrs children = elem "td" attrs children
let textarea attrs children = elem "textarea" attrs children
let tfoot attrs children = elem "tfoot" attrs children
let th attrs children = elem "th" attrs children
let thead attrs children = elem "thead" attrs children
let time attrs children = elem "time" attrs children
let tr attrs children = elem "tr" attrs children
let track attrs = elem "track" attrs []
let u attrs children = elem "u" attrs children
let ul attrs children = elem "ul" attrs children
let var attrs children = elem "var" attrs children
let video attrs children = elem "video" attrs children
let wbr attrs = elem "wbr" attrs []
let text_list l = fragment (List.map text l)

let resource ~init ~free use =
  let r = init () in
  let html = use r in
  let unmount () =
    free r;
    html.unmount ()
  in
  { html with unmount }

(* Extra constructors. *)
module Elem = struct
  type t = html

  include Elem_util

  let of_some to_html option =
    match option with
    | Some x -> to_html x
    | None -> empty

  let of_ok to_html result =
    match result with
    | Ok x -> to_html x
    | Error _ -> empty

  let make ~mount ~unmount () = { mount; unmount }
  let list f list = fragment (List.map f list)
  let list_indexed f list = fragment (List.mapi f list)

  let on_unmount f t =
    {
      t with
      unmount =
        (fun () ->
          f ();
          t.unmount ()
        );
    }

  let unsafe name attrs content =
    let node = Dom.Document.create_element name in
    Dom.Node.set_inner_html node content;
    let mount ~parent:_ ~insert =
      insert node;
      List.iter (fun attr -> Attr.set attr node) attrs
    in
    let unmount () =
      match Dom.Node.parent node with
      | Some parent -> Dom.Node.remove_child ~parent node
      | None ->
        failwith
          "bug: attempting to remove an HTML element node without a parent."
    in
    { mount; unmount }
end

(*
module Head = struct
  let title attrs children = elem "title" attrs children
  let style attrs children = elem "style" attrs children
  let body attrs children = elem "body" attrs children
  let link attrs = elem "link" attrs []
  let noscript attrs children = elem "noscript" attrs children
  let script attrs children = elem "script" attrs children
  let template attrs children = elem "template" attrs children
end 
*)

(* DOM helpers *)

let mount parent html = Elem.mount html ~parent
