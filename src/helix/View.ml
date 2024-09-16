module Comment = Stdweb.Dom.Comment
module Fragment = Stdweb.Dom.Fragment
module Document = Stdweb.Dom.Document
module Node = Stdweb.Dom.Node
module Event = Stdweb.Dom.Event

module List_ext = struct
  let rev_mapi f l0 =
    let rec loop i acc l =
      match l with
      | [] -> acc
      | x :: l' -> loop (i + 1) (f i x :: acc) l'
    in
    loop 0 [] l0
end

let option_get option =
  match option with
  | Some x -> x
  | None -> invalid_arg "option is None"

let or_fail msg option =
  match option with
  | Some x -> x
  | None -> failwith msg

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
      [
        Html.style_list
          [ ("display", "flex"); ("flex-direction", "column"); ("border", "2px solid " ^ c) ];
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
          [ Html.text comment_data; Html.text "#"; Html.int render_count ];
        html;
      ]

let debug_html : (int -> string -> Html.html -> Html.html) ref = ref fake_debug_html
let enable_debug flag = debug_html := if flag then real_debug_html else fake_debug_html

let gen_show_id =
  let i = ref (-1) in
  fun label ->
    incr i;
    String.concat ""
      [
        "show:";
        string_of_int !i;
        (match label with
        | None -> ""
        | Some x -> "/" ^ x);
      ]

let show ?label (to_html : 'a -> Html.html) signal : Html.html =
 fun ctx parent ->
  let count = ref 0 in
  let comment_data = gen_show_id label in
  let anchor = Comment.make comment_data in
  let this_ctx = Html.Ctx.make () in
  let curr_state =
    let html = to_html (Signal.get signal) in
    let html = !debug_html !count comment_data html in
    incr count;
    ref (html this_ctx parent)
  in
  let unsub =
    let insert = Node.insert_after ~parent ~reference:anchor in
    Signal.sub'
      (fun x ->
        let html = to_html x in
        let html = !debug_html !count comment_data html in
        incr count;
        let next_state = html this_ctx parent in
        !curr_state.unmount ();
        next_state.mount insert;
        curr_state := next_state)
      signal
  in
  Html.Ctx.on_cleanup this_ctx unsub;
  let mount insert =
    insert anchor;
    !curr_state.mount insert;
    Html.Ctx.link ctx this_ctx
  in
  let unmount () =
    Node.remove anchor;
    !curr_state.unmount ();
    Html.Ctx.cleanup this_ctx;
    Html.Ctx.unlink ctx this_ctx
  in
  { mount; unmount }

let show_some ?label to_html opt_signal =
  show ?label
    (function
      | None -> Html.null
      | Some x -> to_html x)
    opt_signal

let show_ok ?label to_html res_signal =
  show ?label
    (function
      | Error _ -> Html.null
      | Ok x -> to_html x)
    res_signal

(* Conditional *)

let gen_conditional_id =
  let i = ref (-1) in
  fun () ->
    incr i;
    "conditional:" ^ string_of_int !i

let conditional ~on:pred signal html : Html.html =
  show ~label:"conditional" (fun x -> if pred x then html else Html.null) signal

(* Each *)

module Each_cache : sig
  type t
  type slots
  type key

  val key : 'a -> key
  val make : unit -> t
  val set : t -> key:key -> slots -> unit
  val get : t -> key:key -> slots option
  val get_first_slot : slots -> int * Html.Elem.state
  val add_slot : t -> key:key -> int -> Html.Elem.state -> unit
  val del_slot : t -> key:key -> slots -> int -> unit
  val clear : t -> unit
end = struct
  module Map = Stdweb.Map
  module Iterator = Stdweb.Iterator
  module Dict = Stdweb.Dict

  type key = Jx.t
  type slots = Html.Elem.state Map.t
  type t = slots Map.t

  let key x = Jx.Encoder.any x
  let make = Map.make
  let make_slots = Map.make

  let get_first_slot (slots : slots) =
    match Map.first_key slots with
    | None -> failwith "BUG: get_slot: slots must not be empty"
    | Some idx_js ->
      let idx = Jx.Decoder.int idx_js in
      let html = Map.get slots idx_js |> Option.get in
      (idx, html)

  let set cache ~key (slots : slots) = Map.set cache key slots
  let get (cache : t) ~key = Map.get cache key

  let add_slot (cache : t) ~key idx html =
    let slots : slots =
      match get cache ~key with
      | None -> make_slots ()
      | Some slots -> slots
    in
    Map.set slots (Jx.Encoder.int idx) html;
    set cache ~key slots

  let del_slot cache ~key slots idx =
    Map.delete slots (Jx.Encoder.int idx);
    if Map.size slots = 0 then Map.delete cache key

  let clear cache =
    Map.values cache
    |> Iterator.iter (fun (slots : slots) ->
           let states = Map.values slots in
           Iterator.iter (fun (s : Html.Elem.state) -> s.unmount ()) states;
           Map.clear slots)
end

let gen_each_id =
  let i = ref (-1) in
  fun () ->
    incr i;
    "each:" ^ string_of_int !i

external default_key : 'a -> string = "%identity"

let each ?key:(to_key = default_key) (to_html : 'a -> Html.html) items_signal : Html.html =
 fun ctx parent ->
  let anchor = Comment.make (gen_each_id ()) in
  let frag = Fragment.make () in
  let curr_cache = ref (Each_cache.make ()) in
  let this_ctx = Html.Ctx.make () in
  let unsub =
    Signal.sub'
      (fun new_items ->
        let next_cache = Each_cache.make () in
        List.iteri
          (fun j item ->
            let key = Each_cache.key (to_key item) in
            match Each_cache.get !curr_cache ~key with
            | None ->
              let html : Html.html = to_html item in
              let state = html this_ctx parent in
              state.mount (Node.append_child ~parent:frag);
              Each_cache.add_slot next_cache ~key j state
            | Some old_slots ->
              let i, i_state = Each_cache.get_first_slot old_slots in
              i_state.mount (Node.append_child ~parent:frag);
              Each_cache.del_slot !curr_cache ~key old_slots i;
              Each_cache.add_slot next_cache ~key j i_state)
          new_items;
        Each_cache.clear !curr_cache;
        Node.insert_after ~parent ~reference:anchor frag;
        curr_cache := next_cache)
      items_signal
  in
  Html.Ctx.on_cleanup this_ctx unsub;
  let mount insert =
    insert anchor;
    let items = Signal.get items_signal in
    List.iteri
      (fun i item ->
        let html : Html.html = to_html item in
        let state = html this_ctx parent in
        state.mount (Node.append_child ~parent:frag);
        let key = Each_cache.key item in
        Each_cache.add_slot !curr_cache ~key i state)
      items;
    Node.insert_after ~parent ~reference:anchor frag;
    Html.Ctx.link ctx this_ctx
  in
  let unmount () =
    Each_cache.clear !curr_cache;
    Html.Ctx.cleanup this_ctx;
    Html.Ctx.unlink ctx this_ctx
  in
  { mount; unmount }

(* Bind *)

let bind to_attr signal ctx node =
  let curr_state = ref { Html.Attr.set = ignore; unset = ignore } in
  let unsub =
    Signal.sub'
      (fun x ->
        !curr_state.unset ();
        let next_attr : Html.attr = to_attr x in
        let next_state = next_attr ctx node in
        next_state.set ();
        curr_state := next_state)
      signal
  in
  Html.Ctx.on_cleanup ctx unsub;
  let set () =
    let next_attr : Html.attr = to_attr (Signal.get signal) in
    let next_state = next_attr ctx node in
    next_state.set ();
    curr_state := next_state
  in
  let unset () = !curr_state.unset () in
  { Html.Attr.set; unset }

let bind_some to_attr opt_signal =
  bind
    (function
      | None -> Html.Attr.nop
      | Some x -> to_attr x)
    opt_signal

let bind_ok to_attr res_signal =
  bind
    (function
      | Error _ -> Html.Attr.nop
      | Ok x -> to_attr x)
    res_signal

(* Toggle *)

let toggle ~on:pred signal attr : Html.attr =
 fun ctx node ->
  let active_sig = Signal.uniq ~equal:( == ) (Signal.map pred signal) in
  let state : Html.Attr.state = attr ctx node in
  let is_active = ref false in
  let unsub =
    Signal.use'
      (fun active ->
        if active then state.set () else state.unset ();
        is_active := active)
      active_sig
  in
  Html.Ctx.on_cleanup ctx unsub;
  let set () =
    is_active := Signal.get active_sig;
    if !is_active then state.set () else state.unset ()
  in
  let unset () = if !is_active then state.unset () in
  { Html.Attr.set; unset }

(* Visible *)

let visible ~on:pred signal : Html.Attr.t =
  toggle ~on:(fun x -> not (pred x)) signal (Html.style_list [ ("display", "none") ])
