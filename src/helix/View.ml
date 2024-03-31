module Comment = Stdweb.Dom.Comment
module Fragment = Stdweb.Dom.Fragment
module Document = Stdweb.Dom.Document
module Node = Stdweb.Dom.Node
module Event = Stdweb.Dom.Event

let option_get option =
  match option with
  | Some x -> x
  | None -> invalid_arg "option is None"

let insert_after_anchor ~parent ~anchor node =
  match Node.next_sibling anchor with
  | Some anchor_sibling ->
    Node.insert_before ~parent ~reference:anchor_sibling node
  | None -> Node.append_child ~parent node

(* Show *)

let debug_html ~render_count:_ ~comment_data:_ x = x

let _debug_html =
  let colors =
    [|
      "magenta"; "cyan"; "salmon"; "aquamarine"; "lime"; "yellow"; "palegreen";
    |]
  in
  let i = ref (-1) in
  let get_color () =
    if !i + 1 >= Array.length colors then i := 0 else incr i;
    Array.get colors !i
  in
  fun ~render_count ~comment_data html ->
    let c = get_color () in
    Html.div
      [
        Html.style_list
          [
            ("display", "flex");
            ("flex-direction", "column");
            ("border", "2px solid " ^ c);
          ];
      ]
      [
        Html.span
          [
            Html.style_list
              [
                ("background-color", c);
                ("font-size", "small");
                ("font-weight", "bold");
                ("font-family", "courier");
              ];
          ]
          [ Html.text_list [ comment_data; "#"; string_of_int render_count ] ];
        html;
      ]

let gen_show_id =
  let i = ref (-1) in
  fun label ->
    incr i;
    "show:"
    ^ string_of_int !i
    ^
    match label with
    | None -> ""
    | Some x -> "/" ^ x

type show = {
  mutable render_count : int;
  mutable prev : Html.html;
  mutable unsub : unit -> unit;
  mutable mounted : bool;
}

let init_show () =
  { render_count = 0; prev = Html.empty; unsub = ignore; mounted = false }

let reset_show show =
  show.render_count <- 0;
  show.prev <- Html.empty;
  show.unsub <- ignore;
  show.mounted <- false

let show ?label (to_html : 'a -> Html.html) signal : Html.html =
  let comment_data = gen_show_id label in
  let anchor = Comment.make comment_data in
  let state = init_show () in
  let mount ~parent ~insert =
    state.mounted <- true;
    insert anchor;
    state.unsub <-
      Signal.use' ~label:comment_data
        (fun x ->
          if not state.mounted then
            failwith
              ("bug: signal leak: sub update for unmounted show elem: "
              ^ comment_data
              ^ ", signal: "
              ^ Signal.label signal
              );
          let next =
            to_html x
            |> debug_html ~render_count:state.render_count ~comment_data
          in
          Html.Elem.unmount state.prev;
          Html.Elem.mount ~parent
            ~insert:(insert_after_anchor ~parent ~anchor)
            next;
          state.prev <- next;
          state.render_count <- state.render_count + 1
        )
        signal
  in
  let unmount () =
    if not state.mounted then
      failwith ("bug: called unmount on not mounted show elem: " ^ comment_data);
    state.unsub ();
    Html.Elem.unmount state.prev;
    Node.remove anchor;
    reset_show state
  in
  Html.Elem.make ~mount ~unmount ()

let show_some ?label to_html opt_signal =
  show ?label
    (function
      | None -> Html.empty
      | Some x -> to_html x
      )
    opt_signal

let show_ok ?label to_html res_signal =
  show ?label
    (function
      | Error _ -> Html.empty
      | Ok x -> to_html x
      )
    res_signal

(* Conditional *)

let gen_conditional_id =
  let i = ref (-1) in
  fun () ->
    incr i;
    "conditional:" ^ string_of_int !i

let conditional ~on:active_sig : Html.Attr.t =
  (* Dedup the boolean signal. *)
  let active_sig = Signal.uniq ~equal:( == ) active_sig in

  (* Anchor for the conditional node. *)
  let anchor = Comment.make (gen_conditional_id ()) in

  (* Initial state. *)
  let should_activate0 = Signal.get active_sig in

  let set node =
    let parent =
      match Node.parent node with
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
        else Node.replace_child ~parent ~reference:node anchor
      )
      active_sig
  in
  let unset node =
    let parent =
      match Node.parent node with
      | Some parent -> parent
      | None ->
        failwith "[BUG]: View.conditional: element does not have a parent"
    in
    (* Put to the original state. *)
    if not should_activate0 then
      Node.replace_child ~parent ~reference:anchor node
  in
  Html.Attr.make ~set ~unset ()

(* Each *)

let gen_each_id =
  let i = ref (-1) in
  fun () ->
    incr i;
    "each:" ^ string_of_int !i

module Each_cache : sig
  type t
  type slots
  type key

  val key : 'a -> key
  val make : unit -> t
  val set : t -> key:key -> slots -> unit
  val get : t -> key:key -> slots option
  val get_slot : slots -> int * Html.html
  val add_slot : t -> key:key -> int -> Html.html -> unit
  val del_slot : t -> key:key -> slots -> int -> unit
  val clear : t -> unit
end = struct
  module Map = Stdweb.Map
  module Iterator = Stdweb.Iterator
  module Dict = Stdweb.Dict

  type key = string
  type slots = Html.html Map.t
  type t = slots Dict.t

  let key x = string_of_int (Hashtbl.hash x)
  let make () = Dict.empty ()
  let make_slots = Map.make

  let get_slot slots =
    match Map.first_key slots with
    | None -> failwith "BUG: get_slot: slots must not be empty"
    | Some idx_js ->
      let idx = Jx.Decoder.int idx_js in
      let html = Map.get slots idx_js in
      (idx, html)

  let set cache ~key slots = Dict.set cache key slots
  let get cache ~key = Dict.get_opt cache key

  let add_slot cache ~key idx html =
    let slots =
      match get cache ~key with
      | None -> make_slots ()
      | Some slots -> slots
    in
    Map.set slots (Jx.Encoder.int idx) html;
    set cache ~key slots

  let del_slot cache ~key slots idx =
    Map.delete slots (Jx.Encoder.int idx);
    if Map.size slots = 0 then Dict.del cache key

  let clear cache =
    Dict.iter cache (fun (slots : slots) ->
        let values = Map.values slots in
        Iterator.iter Html.Elem.unmount values;
        Map.clear slots
    )
end

let each (render : 'a -> Html.html) items_signal : Html.html =
  (* Create anchor. *)
  let comment = Comment.make (gen_each_id ()) in
  let anchor = ref comment in

  (* Initialize cache with items0. *)
  let fragment = Fragment.make () in
  let items0 = Signal.get items_signal in

  let old_cache = ref (Each_cache.make ()) in
  List.iteri
    (fun i item ->
      let key = Each_cache.key item in
      let html = render item in
      Html.Elem.mount html ~parent:fragment;
      Each_cache.add_slot !old_cache ~key i html
    )
    items0;

  let mount ~parent ~insert =
    (* Append anchor and initial fragment. *)
    Node.append_child ~parent !anchor;
    Node.append_child ~parent fragment;

    (* Subscribe to changes. *)
    Signal.sub
      (fun new_items ->
        let new_cache = Each_cache.make () in
        List.iteri
          (fun j item ->
            let key = Each_cache.key item in
            match Each_cache.get !old_cache ~key with
            | None ->
              (* New. *)
              let html = render item in
              Html.Elem.mount html ~parent:fragment;
              Each_cache.add_slot new_cache ~key j html
            | Some old_slots ->
              let i, i_html = Each_cache.get_slot old_slots in
              if i = j then begin
                (* Keep. *)
                anchor := Node.next_sibling !anchor |> option_get;
                Each_cache.del_slot !old_cache ~key old_slots j;
                Each_cache.add_slot new_cache ~key j i_html
              end
              else begin
                (* Swap. *)
                Html.Elem.mount i_html ~parent:fragment;
                Each_cache.del_slot !old_cache ~key old_slots i;
                Each_cache.add_slot new_cache ~key j i_html
              end
          )
          new_items;
        Each_cache.clear !old_cache;
        insert_after_anchor ~parent ~anchor:!anchor fragment;
        old_cache := new_cache;
        anchor := comment
      )
      items_signal
  in
  let unmount () = Each_cache.clear !old_cache in
  Html.Elem.make ~mount ~unmount ()

(* Bind *)

let bind to_attr signal : Html.Attr.t =
  let prev = ref Html.Attr.empty in
  let unsub = ref ignore in
  let set elem =
    unsub :=
      Signal.use'
        (fun x ->
          let next = to_attr x in
          Html.Attr.unset !prev elem;
          Html.Attr.set next elem;
          prev := next
        )
        signal
  in
  let unset elem =
    Html.Attr.unset !prev elem;
    Signal.unsub !unsub;
    unsub := ignore;
    prev := Html.Attr.empty
  in
  Html.Attr.make ~set ~unset ()

let bind_some to_attr opt_signal =
  bind
    (function
      | None -> Html.Attr.empty
      | Some x -> to_attr x
      )
    opt_signal

let bind_ok to_attr res_signal =
  bind
    (function
      | Error _ -> Html.Attr.empty
      | Ok x -> to_attr x
      )
    res_signal

(* Toggle *)

let toggle' ~on:active_sig attr : Html.Attr.t =
  let active_sig = Signal.uniq ~equal:( = ) active_sig in
  let should_activate0 = Signal.get active_sig in
  let set node =
    if should_activate0 then Html.Attr.set attr node;
    Signal.use
      (fun should_activate ->
        if should_activate then Html.Attr.set attr node
        else Html.Attr.unset attr node
      )
      active_sig
  in
  let unset node = Html.Attr.unset attr node in
  Html.Attr.make ~set ~unset ()

let toggle ~on:pred attr s = toggle' ~on:(Signal.map pred s) attr

(* Visible *)

let visible ~on:cond : Html.Attr.t =
  toggle' ~on:(Signal.map not cond) (Html.style_list [ ("display", "none") ])

(* Sync *)

(*
let sync ~on:event_name ~get ~set signal =
  let bind_attr = bind (fun x -> Html.value (get x)) signal in
  let on_attr =
    Html.on event_name (fun event ->
        let value = Node.get_value (Event.target event) in
        Signal.update (fun x -> set x value) signal
    )
  in
  Html.Attr.combine bind_attr on_attr
*)
