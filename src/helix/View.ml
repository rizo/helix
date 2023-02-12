module Attr = Html.Attr
module Node = Stdweb.Dom.Node
module Element = Stdweb.Dom.Element
module Comment = Stdweb.Dom.Comment
module Document_fragment = Stdweb.Dom.Document_fragment
module Document = Stdweb.Dom.Document
module Html_element = Stdweb.Dom.Html_element

let log = Stdweb.Console.log

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

let show (to_html : 'a -> Html.html) signal : Html.html =
  (* Anchor for the show node. *)
  let anchor = Comment.as_node (Comment.make (gen_show_id ())) in

  (* Create initial html. *)
  let init = to_html (Signal.get signal) in
  let init = Html.Node.Internal.of_html init in

  (* Store reference to prev html. *)
  let prev = ref init in

  (* Temporary fragment for next node. *)
  let fragment = Document_fragment.(as_node (make ())) in

  let mount parent =
    (* Add anchor. *)
    Node.append_child ~parent anchor;

    (* Add initial html value. *)
    init.mount parent;

    (* Subscribe to updates. *)
    Signal.sub
      (fun x ->
        (* Create next html. *)
        let next = to_html x in
        let next = Html.Node.Internal.of_html next in

        (* Remove prev node. *)
        !prev.remove ();

        (* Update prev. *)
        prev := next;

        (* Insert next to fragment and the fragment to parent. *)
        next.mount fragment;
        insert_after_anchor ~parent ~anchor fragment)
      signal
  in
  let remove () = !prev.remove () in
  Html.Node.Internal.to_html { mount; remove }

let gen_conditional_id =
  let i = ref (-1) in
  fun () ->
    incr i;
    "conditional:" ^ string_of_int !i

let conditional_attr active_sig : Attr.t =
  (* Dedup the boolean signal. *)
  let active_sig = Signal.uniq ~equal:( == ) active_sig in

  (* Anchor for the conditional node. *)
  let anchor = Comment.as_node (Comment.make (gen_conditional_id ())) in

  (* Initial state. *)
  let should_activate0 = Signal.get active_sig in

  let set elem =
    let node = Element.as_node elem in
    let parent =
      match Node.parent_node node with
      | Some parent -> parent
      | None ->
        failwith "[BUG]: View.conditional: element does not have a parent"
    in

    (* The node is mounted initially. Do we unmount? *)
    if not should_activate0 then
      Node.replace_child ~parent ~reference:node anchor;

    (* Subscribe to updates. *)
    Signal.sub
      (fun should_activate ->
        if should_activate then
          Node.replace_child ~parent ~reference:anchor node
        else Node.replace_child ~parent ~reference:node anchor)
      active_sig
  in
  let remove elem =
    let node = Element.as_node elem in
    let parent =
      match Node.parent_node node with
      | Some parent -> parent
      | None ->
        failwith "[BUG]: View.conditional: element does not have a parent"
    in
    (* Put to the original state. *)
    if not should_activate0 then
      Node.replace_child ~parent ~reference:anchor node
  in
  Attr.Internal.to_attr { set; remove }

let conditional_html ~on:condition_s html = assert false
(* let anchor = Comment.as_node (Comment.make (gen_conditional_id ())) in
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
*)

module Each = struct
  let gen_id =
    let i = ref (-1) in
    fun () ->
      incr i;
      "each:" ^ string_of_int !i

  module Cache : sig
    type t
    type slots
    type key

    val key : 'a -> key
    val make : unit -> t
    val set : t -> key:key -> slots -> unit
    val get : t -> key:key -> slots option
    val get_slot : slots -> int * Html.Node.Internal.t
    val add_slot : t -> key:key -> int -> Html.Node.Internal.t -> unit
    val del_slot : t -> key:key -> slots -> int -> unit
    val clear : t -> unit
  end = struct
    type key = Metajs.js
    type slots = Stdweb.Map.t
    type t = Metajs.js

    let key x = Metajs.js_of_int (Hashtbl.hash x)
    let make () = Metajs.obj [||]
    let make_slots = Stdweb.Map.make
    let js_of_html_internal : Html.Node.Internal.t -> Metajs.js = Obj.magic
    let html_internal_of_js : Metajs.js -> Html.Node.Internal.t = Obj.magic

    let get_slot slots =
      let iter = Stdweb.Map.keys slots in
      let next = Stdweb.Iterator.next iter in
      if Stdweb.Iterator.next_is_done next then
        failwith "BUG: get_slot: slots must not be empty"
      else
        let idx_js = Stdweb.Iterator.next_value next in
        let idx = Metajs.int_of_js idx_js in
        let html_js = Stdweb.Map.get slots idx_js in
        let html = html_internal_of_js html_js in
        (idx, html)

    let set cache ~key slots = Metajs.set cache key (Stdweb.Map.to_js slots)

    let get cache ~key =
      Metajs.option_of_js Stdweb.Map.of_js (Metajs.get cache key)

    let add_slot cache ~key idx html =
      let slots =
        match get cache ~key with
        | None -> make_slots ()
        | Some slots -> slots
      in
      Stdweb.Map.set slots (Metajs.js_of_int idx) (js_of_html_internal html);
      set cache ~key slots

    let del_slot cache ~key slots idx =
      Stdweb.Map.delete slots (Metajs.js_of_int idx);
      if Stdweb.Map.size slots = 0 then Metajs.del cache key

    let clear cache =
      let entries = Stdweb.Object.entries cache in
      Array.iter
        (fun (_key, slots_js) ->
          let slots = Stdweb.Map.of_js slots_js in
          let values = Stdweb.Map.values slots in
          Stdweb.Iterator.iter
            (fun html_js ->
              let html = html_internal_of_js html_js in
              html.remove ())
            values;
          Stdweb.Map.clear slots)
        entries
  end

  let make (render : 'a -> Html.html) items_signal : Html.html =
    (* Create anchor. *)
    let comment = Comment.as_node (Comment.make (gen_id ())) in
    let anchor = ref comment in

    (* Initialize cache with items0. *)
    let fragment = Document_fragment.(as_node (make ())) in
    let items0 = Signal.get items_signal in

    let old_cache = ref (Cache.make ()) in
    List.iteri
      (fun i item ->
        let key = Cache.key item in
        let html = Html.Node.Internal.of_html (render item) in
        html.mount fragment;
        Cache.add_slot !old_cache ~key i html)
      items0;

    let mount parent =
      (* Append anchor and initial fragment. *)
      Node.append_child ~parent !anchor;
      Node.append_child ~parent fragment;

      (* Subscribe to changes. *)
      Signal.sub
        (fun new_items ->
          let new_cache = Cache.make () in
          List.iteri
            (fun j item ->
              let key = Cache.key item in
              match Cache.get !old_cache ~key with
              | None ->
                (* New. *)
                let html = Html.Node.Internal.of_html (render item) in
                html.mount fragment;
                Cache.add_slot new_cache ~key j html
              | Some old_slots ->
                let i, i_html = Cache.get_slot old_slots in
                if i = j then begin
                  (* Keep. *)
                  anchor := Node.next_sibling !anchor |> Option.get;
                  Cache.del_slot !old_cache ~key old_slots j;
                  Cache.add_slot new_cache ~key j i_html
                end
                else begin
                  (* Swap. *)
                  i_html.mount fragment;
                  Cache.del_slot !old_cache ~key old_slots i;
                  Cache.add_slot new_cache ~key j i_html
                end)
            new_items;
          Cache.clear !old_cache;
          insert_after_anchor ~parent ~anchor:!anchor fragment;
          old_cache := new_cache;
          anchor := comment)
        items_signal
    in
    let remove () = Cache.clear !old_cache in
    Html.Node.Internal.to_html { mount; remove }
end

let each = Each.make

(* Assign *)

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
