open struct
  module String_map = Map.Make (String)

  type 'a signal = 'a Signal.t

  let ( or ) opt default =
    match opt with
    | Some x -> x
    | None -> default

  let or_fail msg opt =
    match opt with
    | Some x -> x
    | None -> failwith msg
end

type 'a var = {
  label : string;
  to_string : 'a -> string;
  of_string : string -> 'a option;
  equal : 'a -> 'a -> bool;
}

let var ~of_string ~to_string ?(equal = Stdlib.( = )) label =
  { label; to_string; of_string; equal }

let int =
  {
    label = "int";
    to_string = string_of_int;
    of_string = int_of_string_opt;
    equal = Int.equal;
  }

let string =
  {
    label = "string";
    to_string = Fun.id;
    of_string = Option.some;
    equal = String.equal;
  }

let query =
  {
    label = "query";
    to_string = Stdweb.Url_search_params.to_string;
    of_string = (fun str -> Some (Stdweb.Url_search_params.of_string str));
    equal =
      (fun x1 x2 ->
        String.equal
          (Stdweb.Url_search_params.to_string x1)
          (Stdweb.Url_search_params.to_string x2)
      );
  }

type t = { prefix : string list signal; rest : string list signal }

type ('view, 'link, 'a) path =
  | Const : string * ('view, 'link, 'a) path -> ('view, 'link, 'a) path
  | Var :
      'v var * 'v signal option * ('view, 'link, 'a) path
      -> ('v signal -> 'view, 'v -> 'link, 'a) path
  | Rest : (t -> 'a, ('view, 'link, 'a) path -> 'link, 'a) path
  | End : (unit -> 'a, 'a, 'a) path

type route =
  | Route : ('view, 'link, Html.elem) path * 'view -> route
  | Alias : (unit -> 'a, 'a, 'a) path * string list -> route

type lookup = { route : route; matched : string list; args : string list }
(* Routing table lookup result. *)

type capture = Match of route | No_match | Partial of route
(* What does a path in a routing table capture?. *)

type table = { capture : capture; children : table String_map.t }

let route path render = Route (path, render)

let string_of_path : type a k out. (a, k, out) path -> string =
 fun path0 ->
  let rec loop : type a k out. (a, k, out) path -> string =
   fun path ->
    match path with
    | End -> ""
    | Rest -> "**"
    | Const (const, End) -> const
    | Const (const, tail) -> String.concat "/" [ const; loop tail ]
    | Var (var, _, End) -> ":" ^ var.label
    | Var (var, _, tail) -> String.concat "/" [ var.label; loop tail ]
  in
  match path0 with
  | End -> "/"
  | _ -> String.concat "" [ "/"; loop path0 ]

module Table = struct
  type t = table

  let empty = { capture = No_match; children = String_map.empty }

  let register route0 root =
    let rec loop : type a k out. (a, k, out) path -> t -> t =
     fun path node ->
      match path with
      | End -> { node with capture = Match route0 }
      | Rest -> { node with capture = Partial route0 }
      | Const (const, tail) ->
        let matched_node =
          match String_map.find_opt const node.children with
          | Some existing_node -> existing_node
          | None -> { capture = No_match; children = String_map.empty }
        in
        let tail = loop tail matched_node in
        { node with children = String_map.add const tail node.children }
      | Var (_, _, tail) ->
        let matched_node =
          match String_map.find_opt ":" node.children with
          | Some existing_node -> existing_node
          | None -> { capture = No_match; children = String_map.empty }
        in
        let tail = loop tail matched_node in
        { node with children = String_map.add ":" tail node.children }
    in
    match route0 with
    | Route (path0, _) -> loop path0 root
    | Alias (path0, _) -> loop path0 root

  exception Incomplete_match of string list
  exception No_match of string list

  let lookup (table0 : t) (input0 : string list) : (lookup, exn) result =
    let rec loop node input matched args =
      match input with
      | [] -> (
        match node.capture with
        | No_match -> Error (Incomplete_match input0)
        | Match route ->
          Ok { route; matched = List.rev matched; args = List.rev args }
        | Partial route ->
          Ok { route; matched = List.rev matched; args = List.rev args }
      )
      | input_hd :: input' -> (
        (* Const *)
        match String_map.find_opt input_hd node.children with
        | Some node' -> loop node' input' (input_hd :: matched) args
        | None -> (
          (* Rest *)
          match node.capture with
          | Partial route ->
            Ok
              {
                route;
                matched = List.rev matched;
                args = List.rev args @ input;
              }
          | _ -> (
            (* Var *)
            match String_map.find_opt ":" node.children with
            | Some node' ->
              loop node' input' (input_hd :: matched) (input_hd :: args)
            | None -> Error (No_match input0)
          )
        )
      )
    in
    loop table0 input0 [] []

  let of_route_list routes =
    List.fold_left (fun acc x -> register x acc) empty routes
end

let table = Table.of_route_list
let make ?(prefix = Signal.make []) rest = { prefix; rest }
let path (router : t) = router.rest
let prefix (router : t) = router.prefix

let rec eval_path :
    type view link a. (string list -> a) -> (view, link, a) path -> link =
 fun k path ->
  match path with
  | End -> k []
  | Rest -> fun path2 -> eval_path k path2
  | Const (const, End) -> k [ const ]
  | Const (const, tail) -> eval_path (fun rest -> k (const :: rest)) tail
  | Var (var, _, tail) ->
    fun x -> eval_path (fun rest -> k (var.to_string x :: rest)) tail

let location_set_path path =
  let hash = "#/" ^ String.concat "/" path in
  Stdweb.Dom.Location.set_hash Stdweb.Dom.Window.location hash

let alias src dst = eval_path (fun str_path -> Alias (src, str_path)) dst

(* go_up 2 a/b/c/d -> /a/b *)
(* go_up 0 a/b/c/d -> /a/b/c/d *)
(* go_up 2 / -> / *)
(* go_up 10 a/b/c/d -> / *)
let go_up n0 prefix =
  if n0 = 0 then prefix
  else if n0 < 0 then invalid_arg "up must be positive"
  else
    let prefix_rev = List.rev prefix in
    let rec loop n input =
      if n = 0 then List.rev input
      else
        match input with
        | [] -> []
        | _ :: tl -> loop (n - 1) tl
    in
    loop n0 prefix_rev

let go ?(absolute = false) ?(up = 0) (router : t) path =
  eval_path
    (fun str_path ->
      let prefix =
        if absolute then [] else go_up up (Signal.get router.prefix)
      in
      location_set_path (prefix @ str_path)
    )
    path

let pick_qpath segments =
  List.map
    (function
      | Either.Left const -> const
      | Right var_sig -> Signal.get var_sig
      )
    segments

let link ?(absolute = false) ?(active = Html.Attr.empty)
    ?(inactive = Html.Attr.empty) ?(exact = false) ?(up = 0) (router : t) path0
    =
  let check_is_active link curr =
    if exact then String.equal link curr
    else String.starts_with ~prefix:link curr
  in
  eval_path
    (fun str_path ->
      let out =
        Signal.map
          (fun prefix -> (go_up up prefix, str_path))
          (if absolute then Signal.make [] else router.prefix)
      in
      Signal.pair out router.rest
      |> Signal.uniq ~equal:(fun ((p1, s1), r1) ((p2, s2), r2) ->
             if active == Html.Attr.empty then
               List.equal String.equal p1 p2 && List.equal String.equal s1 s2
             else
               List.equal String.equal p1 p2
               && List.equal String.equal s1 s2
               && List.equal String.equal r1 r2
         )
      |> View.bind (fun ((link_prefix, link_suffix), rest) ->
             let path_str =
               String.concat "/" (("#" :: link_prefix) @ link_suffix)
             in
             let href_attr = Html.href path_str in
             let user_attr =
               if active == Html.Attr.empty then inactive
               else
                 let rest_str = String.concat "/" rest in
                 let link_suffix_str = String.concat "/" link_suffix in
                 if check_is_active link_suffix_str rest_str then active
                 else inactive
             in
             Html.Attr.combine href_attr user_attr
         )
    )
    path0

type emits = {
  mutable emit_args : (notify:Signal.notification -> string -> unit) list;
  mutable emit_prefix : notify:Signal.notification -> string list -> unit;
  mutable emit_rest : notify:Signal.notification -> string list -> unit;
}

let init_emits () =
  {
    emit_args = [];
    emit_prefix = (fun ~notify:_ _ -> ());
    emit_rest = (fun ~notify:_ _ -> ());
  }

let apply emits ~prefix:absprefix0 ~matched ~args:args0 =
  let ( let* ) = Result.bind in
  let absprefix_sig =
    Signal.map (fun absprefix0 -> absprefix0 @ matched) absprefix0
  in
  let absprefix_emit ~notify prefix' =
    let absprefix' = Signal.get absprefix0 @ prefix' in
    if not (List.equal String.equal absprefix' (Signal.get absprefix_sig)) then
      Signal.emit ~notify absprefix' absprefix_sig
  in
  emits.emit_prefix <- absprefix_emit;
  let rec loop :
      type view link.
      string list ->
      _ list ->
      (string, string signal) Either.t list ->
      (view, link, Html.elem) path ->
      view ->
      (Html.elem * (string, string signal) Either.t list, string) result =
   fun args args_emits rev_qualified_path path view ->
    match (path, args) with
    | Rest, _ ->
      let rest_sig = Signal.make args in
      let rest_emit ~notify rest =
        if not (List.equal String.equal rest (Signal.get rest_sig)) then
          Signal.emit ~notify rest rest_sig
      in
      emits.emit_args <- List.rev args_emits;
      emits.emit_rest <- rest_emit;
      let html = view ({ prefix = absprefix_sig; rest = rest_sig } : t) in
      let qualified_path' =
        let qualified_rest = List.map (fun arg -> Either.Left arg) args in
        List.rev_append rev_qualified_path qualified_rest
      in
      Ok (html, qualified_path')
    | End, [] ->
      emits.emit_args <- List.rev args_emits;
      Ok (view (), List.rev rev_qualified_path)
    | End, _ :: _ -> Error "too many arguments for path"
    | Const (const, path'), _ ->
      loop args args_emits (Left const :: rev_qualified_path) path' view
    | Var _, [] -> Error "insufficient arguments for path"
    | Var (var, var_sig_opt, path'), arg_str :: args' ->
      let* arg =
        var.of_string arg_str
        |> Option.to_result
             ~none:
               (String.concat ""
                  [ "could not decode "; var.label; " variable: "; arg_str ]
               )
      in
      let arg_sig =
        match var_sig_opt with
        | None -> Signal.make ~label:("var-" ^ var.label) arg
        | Some var_sig ->
          Signal.emit arg var_sig;
          var_sig
      in
      let arg_emit ~notify arg_str' =
        let arg' =
          var.of_string arg_str' |> or_fail ("var decoding failed: " ^ var.label)
        in
        if not (var.equal (Signal.get arg_sig) arg') then
          Signal.emit ~notify arg' arg_sig
      in
      let qualified_path' =
        Either.Right (Signal.map var.to_string arg_sig) :: rev_qualified_path
      in
      loop args' (arg_emit :: args_emits) qualified_path' path' (view arg_sig)
  in
  loop args0 [] []

let render_lookup_error ~prefix ?alias ~label ~default err =
  let label =
    match alias with
    | None -> label
    | Some alias -> String.concat "" [ label; ": alias "; alias ]
  in
  match err with
  | Table.No_match path ->
    default
    or Html.text
         (label ^ ": no match: /" ^ String.concat "/" (Signal.get prefix @ path))
  | Table.Incomplete_match path ->
    default
    or Html.text
         (label
         ^ ": incomplete match: /"
         ^ String.concat "/" (Signal.get prefix @ path)
         )
  | exn ->
    Jx.log exn;
    Html.text (label ^ ": unexpected exception")

(* TODO: must be lazy initialized similar to View.show. *)
(* TODO: improve exn context logging. *)
let dispatch_table ?label ?default ({ prefix; rest } : t) table : Html.elem =
 fun parent insert ->
  let label =
    match label with
    | None -> "router"
    | Some x -> "router-" ^ x
  in
  let lookup_sig = Signal.map (Table.lookup table) rest in
  let lookup_route_sig =
    Signal.uniq
      ~equal:(fun res1 res2 ->
        match (res1, res2) with
        | Ok { route = r1; _ }, Ok { route = r2; _ } -> r1 == r2
        | _ -> false
      )
      lookup_sig
  in
  (* Do we need to clean up emits in unmount? *)
  let emits = init_emits () in
  let html =
    View.show ~label
      (function
        | Ok { route = Alias (src_path, dst_str_path); _ } -> begin
          match Table.lookup table dst_str_path with
          | Ok { route = Route (path, view); matched; args } -> begin
            match apply emits ~prefix ~matched ~args path view with
            | Ok (html, _qpath) -> html
            | Error err -> Html.text (label ^ ": " ^ err)
          end
          | Ok { route = Alias _; _ } -> failwith "bug: forward to forward"
          | Error err ->
            let alias = string_of_path src_path in
            render_lookup_error ~prefix ~alias ~label ~default err
        end
        | Ok { route = Route (path, view); matched; args } -> begin
          match apply emits ~prefix ~matched ~args path view with
          | Ok (html, qpath) ->
            (*Jx.log ("AFTER APPLY", Array.of_list qpath);*)
            (*let up_rest = Signal.make [] in
              List.iter
                (fun seg ->
                  match seg with
                  | Either.Left _ -> ()
                  | Either.Right var_sig ->
                    (* FIXME: this qpath is the initial path at rendering time, not the current rest path. *)
                    Signal.sub (fun _ -> Signal.emit (pick_qpath qpath) up_rest) var_sig
                )
                qpath;
              Signal.sub
                (fun rest ->
                  (* Jx.log ("ROUTE UP, rest", Array.of_list rest, Array.of_list (Signal.get router.rest)); *)
                  let hash = "#/" ^ String.concat "/" (Signal.get prefix @ rest) in
                  Stdweb.Dom.Location.set_hash Stdweb.Dom.Window.location hash
                )
                up_rest;*)
            html
          | Error err -> Html.text (label ^ ": " ^ err)
        end
        | Error err -> render_lookup_error ~prefix ~label ~default err
        )
      lookup_route_sig
  in
  let unsub =
    Signal.sub'
      (function
        | Ok ({ args = args0; matched = matched0; route = Route _ } : lookup) ->
          Signal.scope (fun notify ->
              emits.emit_prefix ~notify matched0;
              let rec loop args_emits args =
                match (args_emits, args) with
                | [], [] -> emits.emit_rest ~notify []
                | [], rest -> emits.emit_rest ~notify rest
                | _arg_emits, [] ->
                  invalid_arg "router: insufficient args for emit"
                | arg_emit :: arg_emits', arg :: args' ->
                  (* TODO: these emits can raise var decoding exceptions, handle this here and update view with error. *)
                  arg_emit ~notify arg;
                  loop arg_emits' args'
              in
              loop emits.emit_args args0
          )
        | Ok ({ route = Alias _; _ } : lookup) -> ()
        | Error _ -> ()
        )
      lookup_sig
  in
  let html' = Html.Elem.on_unmount unsub html in
  html' parent insert

let dispatch ?label ?default router routes =
  dispatch_table ?label ?default router (Table.of_route_list routes)
