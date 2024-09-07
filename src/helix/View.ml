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
  | Some anchor_sibling -> Node.insert_before ~parent ~reference:anchor_sibling node
  | None -> Node.append_child ~parent node

(* Show *)

let fake_debug_html _render_count _comment_data html = html

let real_debug_html =
  let colors = [| "magenta"; "cyan"; "salmon"; "aquamarine"; "lime"; "yellow"; "palegreen" |] in
  let i = ref (-1) in
  let get_color () =
    if !i + 1 >= Array.length colors then i := 0 else incr i;
    Array.get colors !i
  in
  fun render_count comment_data html ->
    let c = get_color () in
    Html.div
      [ Html.style_list
          [ ("display", "flex"); ("flex-direction", "column"); ("border", "2px solid " ^ c) ]
      ]
      [ Html.span
          [ Html.style_list
              [ ("background-color", c)
              ; ("font-size", "small")
              ; ("font-weight", "bold")
              ; ("font-family", "courier")
              ]
          ]
          [ Html.text_list [ comment_data; "#"; string_of_int render_count ] ]
      ; html
      ]

let debug_html : (int -> string -> Html.elem -> Html.elem) ref = ref fake_debug_html
let enable_debug flag = debug_html := if flag then real_debug_html else fake_debug_html

let gen_show_id =
  let i = ref (-1) in
  fun label ->
    incr i;
    String.concat ""
      [ "show:"
      ; string_of_int !i
      ; ( match label with
        | None -> ""
        | Some x -> "/" ^ x
        )
      ]

let show ?label (to_html : 'a -> Html.elem) signal : Html.elem =
 fun parent insert ->
  let comment_data = gen_show_id label in
  let anchor = Comment.make comment_data in
  let state = ref { Html.Elem.free = None; remove = ignore } in
  let count = ref 0 in
  insert anchor;
  let unsub =
    Signal.use' ~label:comment_data
      (fun x ->
        let next_html = !debug_html !count comment_data (to_html x) in
        incr count;
        Html.Elem.unmount !state;
        let next_state = next_html parent (Node.insert_after ~parent ~reference:anchor) in
        state := next_state
      )
      signal
  in
  let free () =
    unsub ();
    Option.iter (fun f -> f ()) !state.free
  in
  let remove () =
    !state.remove ();
    Node.remove anchor
  in
  { free = Some free; remove }

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

(* [TODO] on should be a pred fn. *)
let conditional ~on:active_sig node =
  let active_sig = Signal.uniq ~equal:( == ) active_sig in
  let anchor = Comment.make (gen_conditional_id ()) in
  let parent =
    match Node.parent node with
    | None -> failwith "conditional: attribute node has no parent"
    | Some parent -> parent
  in
  let unsub = ref ignore in

  let set () =
    let active0 = Signal.get active_sig in
    if not active0 then Node.replace_child ~parent ~reference:node anchor;

    unsub :=
      Signal.sub'
        (fun active ->
          if active then Node.replace_child ~parent ~reference:anchor node
          else Node.replace_child ~parent ~reference:node anchor
        )
        active_sig
  in
  let unset () =
    let () =
      (* Put the node back, if not mounted. *)
      if Option.is_none (Node.parent node) then Node.replace_child ~parent ~reference:anchor node
    in
    !unsub ();
    (* [IMPORTANT] Must be set to ignore in case free is called. *)
    unsub := ignore
  in
  let free () = !unsub () in
  { Html.Attr.set; unset; free = Some free }

(* Each *)

module Each_cache : sig
  type t
  type slots
  type key

  val key : 'a -> key
  val make : unit -> t
  val set : t -> key:key -> slots -> unit
  val get : t -> key:key -> slots option
  val get_slot : slots -> int * Html.elem
  val add_slot : t -> key:key -> int -> Html.elem -> unit
  val del_slot : t -> key:key -> slots -> int -> unit
  val clear : t -> unit
end = struct
  module Map = Stdweb.Map
  module Iterator = Stdweb.Iterator
  module Dict = Stdweb.Dict

  type key = string
  type slots = Html.elem Map.t
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
        (* Iterator.iter (fun (elem : Html.elem) -> Html.Elem.unmount elem ()) values; *)
        Map.clear slots
    )
end

let gen_each_id =
  let i = ref (-1) in
  fun () ->
    incr i;
    "each:" ^ string_of_int !i

let each (to_html : 'a -> Html.elem) items_signal : Html.elem =
 fun parent insert ->
  (* Create anchor. *)
  let anchor = Comment.make (gen_each_id ()) in

  (* Initialize cache with items0. *)
  (* let fragment = Fragment.make () in *)
  let items0_rev = Signal.get items_signal |> List.rev in

  (* Initial render *)
  insert anchor;
  let states0 =
    List.map
      (fun item ->
        let html = to_html item in
        html parent (Node.insert_after ~parent ~reference:anchor)
      )
      items0_rev
  in

  let states = ref states0 in

  (* Subscribe to changes. *)
  let unsub =
    Signal.sub'
      (fun new_items ->
        let new_items_rev = List.rev new_items in
        List.iter Html.Elem.unmount !states;
        states :=
          List.map
            (fun item ->
              let html = to_html item in
              html parent (Node.insert_after ~parent ~reference:anchor)
            )
            new_items_rev
      )
      items_signal
  in
  let free () =
    List.iter (fun (state : Html.Elem.state) -> Option.iter (fun f -> f ()) state.free) !states;
    unsub ();
    states := []
  in
  let remove () = List.iter (fun (state : Html.Elem.state) -> state.remove ()) !states in
  { free = Some free; remove }

(* Bind *)

let bind to_attr signal node =
  let state = ref { Html.Attr.set = ignore; unset = ignore; free = None } in
  let unsub = ref ignore in
  let set () =
    unsub :=
      Signal.use'
        (fun x ->
          Html.Attr.unset !state;
          let next_state : Html.Attr.state = (to_attr x) node in
          next_state.set ();
          state := next_state
        )
        signal
  in
  let unset () =
    Html.Attr.unset !state;
    Signal.unsub !unsub;
    (* [IMPORTANT] Must be set to ignore in case free is called. *)
    unsub := ignore
  in
  let free () = Signal.unsub !unsub in
  { Html.Attr.set; unset; free = Some free }

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

let toggle' ~on:active_sig attr node =
  let active_sig = Signal.uniq ~equal:( == ) active_sig in
  let state : Html.Attr.state = attr node in
  let unsub = ref ignore in
  let is_active = ref false in
  let set () =
    unsub :=
      Signal.use'
        (fun active ->
          if active then state.set () else state.unset ();
          is_active := active
        )
        active_sig
  in
  let unset () =
    if !is_active then state.unset ();
    !unsub ();
    (* [IMPORTANT] Must be set to ignore in case free is called. *)
    unsub := ignore
  in
  let free () = !unsub () in
  { Html.Attr.set; unset; free = Some free }

let toggle ~on:pred attr s = toggle' ~on:(Signal.map pred s) attr

(* Visible *)

let visible ~on:cond : Html.Attr.t =
  toggle' ~on:(Signal.map not cond) (Html.style_list [ ("display", "none") ])
