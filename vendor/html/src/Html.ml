open Stdweb

module Ctx = struct
  let gen_id =
    let i = ref (-1) in
    fun () ->
      incr i;
      (Obj.magic !i : string)

  type t = { id : string; mutable cleanup : (unit -> unit) list; tree : t Stdweb.Dict.t }

  let make () = { id = gen_id (); cleanup = []; tree = Stdweb.Dict.empty () }
  let on_cleanup t f = t.cleanup <- f :: t.cleanup
  let cleanup t = List.iter (fun c -> c ()) t.cleanup
  let link t t2 = Stdweb.Dict.set t.tree t2.id t2
  let unlink t subctx = Stdweb.Dict.del t.tree subctx.id
end

(* Attributes are functions that set "attributes" on elements.
   Html values are functions that mount and unmount children.

   Cleanup:
   - free must be always called, if defined;
   - remove must be called to deactivate the attr and does not need to be called
     when the node is removed.
*)

module Attr = struct
  type state = { set : unit -> unit; unset : unit -> unit }
  type t = Ctx.t -> Dom.Node.t -> state

  let nop _ctx _node = { set = ignore; unset = ignore }
  let on bool attr = if bool then attr else nop

  let on_some = function
    | None -> nop
    | Some at -> at

  let on_ok = function
    | Error _ -> nop
    | Ok attr -> attr

  let string name value _ctx node =
    let set () = Dom.Node.set_attr node name value in
    let unset () = Dom.Node.unset_attr node name in
    { set; unset }

  let bool name bool = if bool then string name "" else nop
  let int name i = string name (string_of_int i)

  let on_mount f _ctx node =
    let set () = f node in
    { set; unset = ignore }

  let set attr node = attr node
  let unset state = state.unset ()

  let combine a1 a2 ctx node =
    let a1_state = a1 ctx node in
    let a2_state = a2 ctx node in
    let set () =
      a1_state.set ();
      a2_state.set ()
    in
    let unset () =
      a1_state.unset ();
      a2_state.unset ()
    in
    { set; unset }

  let list attrs = List.fold_left combine nop attrs
  let label = string "label"
end

type attr = Attr.t

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

let value x _ctx node =
  let set () = Dom.Node.set_value node x in
  let unset () = Dom.Node.reset_value node in
  { Attr.set; unset }

let value_or default opt =
  match opt with
  | Some x -> value x
  | None -> value default

let wrap value = Attr.string "wrap" value
let width value = Attr.int "width" value
let style x = Attr.string "style" x
let role x = Attr.string "role" x

let style_list items _ctx node =
  let style = Dom.Node.get_style node in
  let set () = List.iter (fun (name, value) -> Dom.Style.set style name value) items in
  let unset () = List.iter (fun (name, _value) -> Dom.Style.unset style name) items in
  { Attr.set; unset }

let class_list items _ctx node =
  let cl = Dom.Node.get_class_list node in
  let set () = List.iter (fun name -> Dom.Token_list.add cl name) items in
  let unset () = List.iter (fun name -> Dom.Token_list.remove cl name) items in
  { Attr.set; unset }

let class_flags options =
  let list = List.fold_left (fun acc (c, b) -> if b then c :: acc else acc) [] options in
  class_list list

(* Do we need to unbind the event explicitly? In theory, the browser should
   remove the listeners when the node is gc'ed. *)
let on ?(default = true) ?confirm (name : Dom.Event.name) f _ctx node =
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
  let set () = Dom.Node.bind node name f' in
  let unset () = Dom.Node.unbind node name f' in
  { Attr.set; unset }

let on_change ?confirm handler =
  on ~default:false ?confirm Dom.Event.change (fun ev ->
      handler (Dom.Node.get_value (Dom.Event.target ev)))

let on_checked ?confirm handler =
  on ~default:false ?confirm Dom.Event.change (fun ev ->
      handler (Dom.Node.get_checked (Dom.Event.target ev)))

let on_input ?confirm handler =
  on ~default:false ?confirm Dom.Event.input (fun ev ->
      handler (Dom.Node.get_value (Dom.Event.target ev)))

let on_click ?confirm handler = on ~default:false ?confirm Dom.Event.click (fun _ -> handler ())

let on_double_click ?confirm handler =
  on ~default:false ?confirm Dom.Event.dblclick (fun _ -> handler ())

(* Elem

    html state is composed of two cleanup functions:
    - free: cleans up any non-dom resources (like signal subscriptions);
    - remove: calls free and also removes the top-level dom resources.

   These two functions are needed since deleting a top-level dom node,
   automatically deletes the children nodes, but will not free any manually
   acquired resources.
*)

(* Extra constructors. *)
module Elem = struct
  type state = { unmount : unit -> unit; mount : (Dom.node -> unit) -> unit }
  type t = Ctx.t -> Dom.node -> state

  let null_state = { unmount = ignore; mount = (fun _insert -> ()) }
  let null _ctx _parent = null_state

  (*
    [NOTE] Invariant
    The parent MUST NOT change.

    [NOTE] Oredr
    The initialization order MUST be:
    1. create elem
    2. insert elem
    3. set attrs
    2. add children

    If this order isn't followed, all kinds of things will break. For example,
    `select` requires that children are present for the `value` attr to work.

    The `conditional` attribute requires that the node is mounted on a parent.
  *)

  let make name attrs children ctx parent =
    let mounted = ref false in
    (* Create node *)
    let node = Dom.Document.create_element name in
    let mount insert =
      insert node;
      if not !mounted then (
        (* Set node's attributes *)
        List.iter
          (fun (attr : Attr.t) ->
            let state = attr ctx node in
            state.set ())
          attrs;
        (* Create and mount children *)
        let node_insert = Dom.Node.append_child ~parent:node in
        List.iter
          (fun (child : t) ->
            let child_state = child ctx node in
            child_state.mount node_insert)
          children;
        mounted := true)
    in
    {
      unmount = (fun () -> Dom.Node.remove_child ~parent node);
      (* Allow mounting node on parent *)
      mount;
    }

  let text data _ctx parent =
    let node = Dom.Document.create_text_node data in
    {
      unmount = (fun () -> Dom.Node.remove_child ~parent node);
      mount = (fun insert -> insert node);
    }

  let of_some to_html option =
    match option with
    | Some x -> to_html x
    | None -> null

  let of_ok to_html result =
    match result with
    | Ok x -> to_html x
    | Error _ -> null

  let on_unmount f elem ctx =
    Ctx.on_cleanup ctx f;
    elem ctx

  let unsafe name attrs content ctx parent =
    let node = Dom.Document.create_element name in
    Dom.Node.set_inner_html node content;
    List.iter
      (fun (attr : Attr.t) ->
        let state = attr ctx node in
        state.set ())
      attrs;
    {
      unmount = (fun () -> Dom.Node.remove_child ~parent node);
      mount = (fun insert -> insert node);
    }

  let fragment children ctx parent =
    let frag = Dom.Fragment.make () in
    let frag_insert = Dom.Node.append_child ~parent:frag in
    let states =
      List.map
        (fun (child : t) ->
          let s = child ctx parent in
          s.mount frag_insert;
          s)
        children
    in
    {
      unmount = (fun () -> List.iter (fun (s : state) -> s.unmount ()) states);
      mount = (fun insert -> insert frag);
    }
end

type html = Elem.t

let elem = Elem.make
let text = Elem.text
let null = Elem.null
let fragment = Elem.fragment
let int n = text (string_of_int n)
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

let on_mount f html ctx parent =
  let state : Elem.state = html ctx parent in
  {
    state with
    mount =
      (fun insert ->
        state.mount insert;
        f ());
  }

let resource ~init ~free (use : 'a -> Elem.t) ctx parent =
  let r = init () in
  let html = use r in
  let state = html ctx parent in
  Ctx.on_cleanup ctx (fun () -> free r);
  state

(* DOM helpers *)

let mount parent html =
  let ctx = Ctx.make () in
  let state : Elem.state = html ctx parent in
  let insert = Dom.Node.append_child ~parent in
  state.mount insert
