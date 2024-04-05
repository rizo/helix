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

let show ?label (to_html : 'a -> Html.html) signal : Html.html =
 fun parent insert ->
  let comment_data = gen_show_id label in
  let anchor = Comment.make comment_data in
  let state = ref { Html.free = None; remove = ignore } in
  insert anchor;
  let unsub =
    Signal.use' ~label:comment_data
      (fun x ->
        let next_html = to_html x in
        Html.Elem.unmount !state;
        let next_state =
          next_html parent (Node.insert_after ~parent ~reference:anchor)
        in
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

let conditional ~on:active_sig : Html.Attr.t =
  (* Dedup the boolean signal. *)
  let active_sig = Signal.uniq ~equal:( == ) active_sig in

  (* Anchor for the conditional node. *)
  let anchor = Comment.make (gen_conditional_id ()) in

  (* Initial state. *)
  let should_activate0 = Signal.get active_sig in

  let unsub = ref ignore in

  let set node =
    (* Initial render *)
    let () =
      match Node.parent node with
      | None -> ()
      | Some parent ->
        if not should_activate0 then
          Node.replace_child ~parent ~reference:node anchor
    in
    (* Subscribe to updates. *)
    unsub :=
      Signal.sub'
        (fun should_activate ->
          match (Node.parent node, Node.parent anchor) with
          | None, None -> ()
          | Some parent, _ | _, Some parent ->
            if should_activate then
              Node.replace_child ~parent ~reference:anchor node
            else Node.replace_child ~parent ~reference:node anchor
        )
        active_sig
  in
  let unset _node = !unsub () in
  Html.Attr.make ~set ~unset ()

(* Each *)

let gen_each_id =
  let i = ref (-1) in
  fun () ->
    incr i;
    "each:" ^ string_of_int !i

let each (to_html : 'a -> Html.html) items_signal : Html.html =
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
    List.iter
      (fun (state : Html.html_state) -> Option.iter (fun f -> f ()) state.free)
      !states;
    unsub ();
    states := []
  in
  let remove () =
    List.iter (fun (state : Html.html_state) -> state.remove ()) !states
  in
  { free = Some free; remove }

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
