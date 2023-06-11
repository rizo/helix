open Stdweb

(* Attributes are functions that set "attributes" on elements. *)
type attr = { set : Dom.Element.t -> unit; remove : Dom.Element.t -> unit }

module Attr = struct
  type t = attr

  let empty = { set = (fun _ -> ()); remove = (fun _ -> ()) }
  let on bool attr = if bool then attr else empty

  let on_some = function
    | None -> empty
    | Some at -> at

  let on_ok = function
    | Error _ -> empty
    | Ok attr -> attr

  let string name value =
    {
      set = (fun el -> Dom.Element.set_attribute el name value);
      remove = (fun el -> Dom.Element.remove_attribute el name);
    }

  let bool name bool = if bool then string name "" else empty
  let int name i = string name (string_of_int i)
  let add el { set; _ } = set el
  let on_mount f = { set = (fun elem -> f elem); remove = (fun _ -> ()) }

  module Internal = struct
    type t = attr = {
      set : Dom.Element.t -> unit;
      remove : Dom.Element.t -> unit;
    }

    let of_attr x = x
    let to_attr x = x
  end
end

let attr = Attr.string

(* Standard constructors. *)
let accept value = Attr.string "accept" value
let accesskey value = Attr.string "accesskey" value
let action value = Attr.string "action" value
let autocomplete value = Attr.string "autocomplete" value
let autofocus = Attr.bool "autofocus" true
let charset value = Attr.string "charset" value
let checked = Attr.bool "checked" true
let class_name value = Attr.string "class" value
let cols value = Attr.int "cols" value
let content value = Attr.string "content" value
let contenteditable value = Attr.bool "contenteditable" value
let defer = Attr.bool "defer" true
let dir value = Attr.string "dir" value
let disabled = Attr.bool "disabled" true
let draggable value = Attr.bool "draggable" value
let for' value = Attr.string "for" value
let height value = Attr.int "height" value
let hidden = Attr.bool "hidden" true
let href value = Attr.string "href" value
let id value = Attr.string "id" value
let lang value = Attr.string "lang" value
let list value = Attr.string "list" value
let media value = Attr.string "media" value
let method' value = Attr.string "method" value
let name value = Attr.string "name" value
let placeholder value = Attr.string "placeholder" value
let rel value = Attr.string "rel" value
let required = Attr.bool "required" true
let rows value = Attr.int "rows" value
let selected = Attr.bool "selected" true
let spellcheck value = Attr.string "spellcheck" value
let src value = Attr.string "src" value
let tabindex value = Attr.int "tabindex" value
let title value = Attr.string "title" value
let type' value = Attr.string "type" value
let value value = Attr.string "value" value
let wrap value = Attr.string "wrap" value
let width value = Attr.int "width" value

let style items =
  let set elem =
    let html_elem = Dom.Html_element.of_element elem in
    List.iter
      (fun (name, value) ->
        Dom.Html_element.set_style_property html_elem name value)
      items
  in
  let remove elem =
    let html_elem = Dom.Html_element.of_element elem in
    List.iter
      (fun (name, _value) ->
        Dom.Html_element.remove_style_property html_elem name)
      items
  in
  { set; remove }

let class_list xs =
  let value = String.concat " " xs in
  Attr.string "class" value

let class_flags options =
  let list =
    List.fold_left (fun acc (c, b) -> if b then c :: acc else acc) [] options
  in
  class_list list

let on kind f =
  let kind = Dom.Event.Kind.to_string kind in
  {
    set = (fun el -> Dom.Element.add_event_listener el kind f);
    remove = (fun el -> Dom.Element.remove_event_listener el kind);
  }

let on_click = on Dom.Event.click
let on_input = on Dom.Event.input
let on_keydown = on Dom.Event.keydown

type html = { mount : Dom.Node.t -> unit; remove : unit -> unit }
type node = Dom.Node.t

let node name (attrs : attr list) (children : node list) : node =
  let elem = Dom.Document.create_element name in
  let node = Dom.Element.to_node elem in

  List.iter
    (fun (child : node) -> Dom.Node.append_child ~parent:node child)
    children;
  List.iter (Attr.add elem) attrs;
  node

let elem name (attrs : attr list) (children : html list) : html =
  let elem = Dom.Document.create_element name in
  let node = Dom.Element.to_node elem in

  let mount parent =
    Dom.Node.append_child ~parent node;
    List.iter (fun (child : html) -> child.mount node) children;
    List.iter (Attr.add elem) attrs
  in

  let remove () =
    match Dom.Node.parent_node node with
    | Some parent -> Dom.Node.remove_child ~parent node
    | None ->
      Console.log
        "BUG: attempting to remove an HTML element that was never added to a \
         parent.";
      Console.log (name, elem)
  in
  { mount; remove }

let text data =
  let node = Dom.Text.to_node (Dom.Document.create_text_node data) in
  let mount parent = Dom.Node.append_child ~parent node in
  let remove () =
    match Dom.Node.parent_node node with
    | Some parent -> Dom.Node.remove_child ~parent node
    | None ->
      Console.log
        "BUG: attempting to remove an HTML text node that was never added to a \
         parent.";
      Console.log (name, node)
  in
  { mount; remove }

let int n = text (string_of_int n)
let empty = { mount = (fun _ -> ()); remove = (fun () -> ()) }
let nbsp = text "\u{00A0}"

(* let fragment children_html parent =
   let node = Dom.Document_fragment.make () |> Dom.Document_fragment.to_node in
   List.iter
     (fun child_html ->
       let _child_node = add_to_parent ~parent:node child_html in
       ())
     children_html;
   node *)

let fragment children =
  let mount parent = List.iter (fun child -> child.mount parent) children in
  let remove () = List.iter (fun child -> child.remove ()) children in
  { mount; remove }

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
let code attrs data = elem "code" attrs [ text data ]
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
let html attrs children = elem "html" attrs children
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

(* Extra constructors. *)
module Elem = struct
  type t = html

  let of_some to_html option =
    match option with
    | Some x -> to_html x
    | None -> empty

  let of_ok to_html result =
    match result with
    | Ok x -> to_html x
    | Error _ -> empty

  module Internal = struct
    type t = html = { mount : Dom.Node.t -> unit; remove : unit -> unit }

    let of_html x = x
    let to_html x = x
  end
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

let render parent html = html.mount (Dom.Element.to_node parent)
