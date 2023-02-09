module Attr = Html.Attr
module Node = Stdweb.Dom.Node
module Element = Stdweb.Dom.Element
module Comment = Stdweb.Dom.Comment
module Document_fragment = Stdweb.Dom.Document_fragment
module Html_element = Stdweb.Dom.Html_element

(* Reactive rendering *)

let gen_show_id =
  let i = ref (-1) in
  fun () ->
    incr i;
    "show:" ^ string_of_int !i

let insert_after_anchor ~parent ~anchor node =
  match Node.next_sibling anchor with
  | Some anchor_sibling ->
    Node.insert_before ~parent ~reference:anchor_sibling node
  | None -> Node.append_child ~parent node

let show (f : 'a -> Html.html) s : Html.html =
  let anchor = Comment.as_node (Comment.make (gen_show_id ())) in
  let fragment = Document_fragment.(as_node (make ())) in
  Node.append_child ~parent:fragment anchor;
  let prev_nodes = ref [] in
  let node0 = Html.Node.Internal.of_html (f (Signal.get s)) in
  Node.append_child ~parent:fragment node0;
  let () =
    let is_fragment =
      Metajs.instanceof (Node.as_js node0) Document_fragment.t
    in
    if is_fragment then
      Node.List.for_each (Node.child_nodes node0) (fun child ->
          prev_nodes := child :: !prev_nodes)
    else prev_nodes := node0 :: !prev_nodes
  in
  Signal.sub
    (fun x ->
      let parent =
        match Node.parent_node anchor with
        | Some parent -> parent
        | None -> failwith "Html.show: cannot update unmounted element"
      in
      let next = Html.Node.Internal.of_html (f x) in
      List.iter (Node.remove_child ~parent) !prev_nodes;
      prev_nodes := [];
      let () =
        let is_fragment =
          Metajs.instanceof (Node.as_js next) Document_fragment.t
        in
        if is_fragment then
          Node.List.for_each (Node.child_nodes next) (fun child ->
              prev_nodes := child :: !prev_nodes)
        else prev_nodes := next :: !prev_nodes
      in
      insert_after_anchor ~parent ~anchor next)
    s;
  Html.Node.Internal.to_html fragment

let show_conditional ?on:(condition_s = Signal.make true) (f : 'a -> Html.html)
    s : Html.html =
  let anchor = Comment.as_node (Comment.make (gen_show_id ())) in
  let fragment = Document_fragment.(as_node (make ())) in
  Node.append_child ~parent:fragment anchor;
  let prev_nodes = ref [] in
  if Signal.get condition_s then begin
    let node0 = Html.Node.Internal.of_html (f (Signal.get s)) in
    Node.append_child ~parent:fragment node0;
    let is_fragment =
      Metajs.instanceof (Node.as_js node0) Document_fragment.t
    in
    if is_fragment then
      Node.List.for_each (Node.child_nodes node0) (fun child ->
          prev_nodes := child :: !prev_nodes)
    else prev_nodes := node0 :: !prev_nodes
  end;
  Signal.sub2
    (fun condition x ->
      let parent =
        match Node.parent_node anchor with
        | Some parent -> parent
        | None -> failwith "Html.show: cannot update unmounted element"
      in

      if condition then (
        let next = Html.Node.Internal.of_html (f x) in
        List.iter (Node.remove_child ~parent) !prev_nodes;
        prev_nodes := [];
        let () =
          let is_fragment =
            Metajs.instanceof (Node.as_js next) Document_fragment.t
          in
          if is_fragment then
            Node.List.for_each (Node.child_nodes next) (fun child ->
                prev_nodes := child :: !prev_nodes)
          else prev_nodes := next :: !prev_nodes
        in
        insert_after_anchor ~parent ~anchor next)
      else (
        List.iter (Node.remove_child ~parent) !prev_nodes;
        prev_nodes := []))
    condition_s s;
  Html.Node.Internal.to_html fragment

let gen_conditional_id =
  let i = ref (-1) in
  fun () ->
    incr i;
    "conditional:" ^ string_of_int !i

let conditional_html ~on:condition_s html =
  let anchor = Comment.as_node (Comment.make (gen_conditional_id ())) in
  let fragment = Document_fragment.(as_node (make ())) in
  Node.append_child ~parent:fragment anchor;
  let prev_ref = ref None in
  if Signal.get condition_s then begin
    let node0 = Html.Node.Internal.of_html html in
    Node.append_child ~parent:fragment node0;
    prev_ref := Some node0
  end;
  Signal.sub
    (fun condition ->
      let parent =
        match Node.parent_node anchor with
        | Some parent -> parent
        | None -> failwith "Html.show: cannot update unmounted element"
      in
      if condition then (
        let next = Html.Node.Internal.of_html html in
        match !prev_ref with
        | None ->
          insert_after_anchor ~parent ~anchor next;
          prev_ref := Some next
        | Some prev ->
          Node.replace_child ~parent ~reference:prev next;
          prev_ref := Some next)
      else
        match !prev_ref with
        | None -> ()
        | Some prev ->
          Node.remove_child ~parent prev;
          prev_ref := None)
    condition_s;
  Html.Node.Internal.to_html fragment

let conditional_attr active_sig : Attr.t =
  let active_sig = Signal.uniq ~equal:( = ) active_sig in
  let should_activate0 = Signal.get active_sig in
  let cache_node = ref (Html.empty ()) in

  let set elem =
    let node = Element.as_node elem in
    (* Make sure it's not a fragment! Why would it be? *)
    let () =
      let is_fragment =
        Metajs.instanceof (Node.as_js node) Document_fragment.t
      in
      if is_fragment then failwith "BUG: conditional attr on fragment!"
    in
    cache_node := node;

    (* if not should_activate0 then Node.remove_child ~parent node; *)

    (* FIXME: Anchor! *)
    Signal.use
      (fun should_activate ->
        let parent =
          match Node.parent_node node with
          | Some parent -> parent
          | None -> failwith "Html.conditional_attr: no parent"
        in

        if should_activate then Node.append_child ~parent node
        else Node.remove_child ~parent node)
      active_sig
  in
  let remove _elem = () in
  Attr.Internal.to_attr { set; remove }

let gen_each_id =
  let i = ref (-1) in
  fun () ->
    incr i;
    "each:" ^ string_of_int !i

let each (to_html : 'a -> Html.html) items_s : Html.html =
  let anchor = Comment.as_node (Comment.make (gen_each_id ())) in
  let fragment = Document_fragment.(as_node (make ())) in
  Node.append_child ~parent:fragment anchor;

  (* Add initial children to parent and cache; set anchor_node. *)
  let items0 = Signal.get items_s in
  let cache = Hashtbl.create (List.length items0) in
  List.iter
    (fun item0 ->
      let hash = Hashtbl.hash item0 in
      let node = Html.Node.Internal.of_html (to_html item0) in
      Node.append_child ~parent:fragment node;
      Hashtbl.add cache hash (node, false))
    items0;

  Signal.sub
    (fun new_items ->
      (* Add new items to fragment; use cache if available; mark new items as fresh. *)
      List.iter
        (fun item ->
          let hash = Hashtbl.hash item in
          match Hashtbl.find_opt cache hash with
          | Some (cached_node, _fresh) ->
            Node.append_child ~parent:fragment cached_node;
            Hashtbl.replace cache hash (cached_node, true)
          | None ->
            let new_node = Html.Node.Internal.of_html (to_html item) in
            Node.append_child ~parent:fragment new_node;
            Hashtbl.add cache hash (new_node, true))
        new_items;

      let parent =
        match Node.parent_node anchor with
        | Some parent -> parent
        | None -> failwith "Html.each: cannot update unmounted elements"
      in

      insert_after_anchor ~parent ~anchor fragment;

      (* Keep only fresh nodes; mark all nodes as not fresh. *)
      Hashtbl.iter
        (fun hash (node, fresh) ->
          if fresh then Hashtbl.replace cache hash (node, false)
          else (
            Node.remove_child ~parent node;
            Hashtbl.remove cache hash))
        cache)
    items_s;
  Html.Node.Internal.to_html fragment

let assign attr_sig : Attr.t =
  let prev0 = Signal.get attr_sig in
  let prev' : Attr.Internal.t ref = ref (Attr.Internal.of_attr prev0) in
  let set elem =
    !prev'.set elem;
    Signal.use
      (fun (next : Attr.t) ->
        let next' = Attr.Internal.of_attr next in
        !prev'.remove elem;
        next'.set elem;
        prev' := next')
      attr_sig
  in
  let remove elem = !prev'.remove elem in
  Attr.Internal.to_attr { set; remove }

let bind to_attr signal : Attr.t =
  let prev0 = to_attr (Signal.get signal) in
  let prev' : Attr.Internal.t ref = ref (Attr.Internal.of_attr prev0) in
  let set elem =
    !prev'.set elem;
    Signal.use
      (fun x ->
        let next' = Attr.Internal.of_attr (to_attr x) in
        !prev'.remove elem;
        next'.set elem;
        prev' := next')
      signal
  in
  let remove elem = !prev'.remove elem in
  Attr.Internal.to_attr { set; remove }

let toggle ~on:active_sig attr0 : Attr.t =
  let active_sig = Signal.uniq ~equal:( = ) active_sig in
  let internal = Attr.Internal.of_attr attr0 in
  let should_activate0 = Signal.get active_sig in

  let set elem =
    if should_activate0 then internal.set elem;
    Signal.use
      (fun should_activate ->
        if should_activate then internal.set elem else internal.remove elem)
      active_sig
  in
  Attr.Internal.to_attr { internal with set }

let visible ~on:cond : Attr.t =
  toggle ~on:(Signal.map not cond) (Html.style [ ("display", "none") ])
