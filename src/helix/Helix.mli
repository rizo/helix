(** Library for building reactive web interfaces.

    {3 Example}

    {[
      open Helix
      open Stdweb

      let counter () =
        let count = Signal.make 0 in
        let open Html in
        div
          [ style_list [ ("border", "1px solid #eee"); ("padding", "1em") ] ]
          [
            h2 [] [ text "Counter" ];
            div [] [ text "Compute a count." ];
            div []
              [
                button [ on_click (fun _ -> Signal.update (fun n -> n + 1) count) ] [ text "+" ];
                button [ on_click (fun _ -> Signal.update (fun n -> n - 1) count) ] [ text "-" ];
                div
                  [
                    style_list [ ("font-size", "32px") ];
                    bind
                      (fun n ->
                        if n < 0 then style_list [ ("color", "red") ]
                        else style_list [ ("color", "blue") ])
                      count;
                  ]
                  [ show (fun n -> text (string_of_int n)) count ];
              ];
          ]

      let () =
        match Dom.Document.get_element_by_id "root" with
        | Some root -> Html.mount root (counter ())
        | None -> failwith "No #root element found"
    ]}*)

type html = Html.html
(** An alias for {!type:Html.html}. *)

type attr = Html.attr
(** An alias for {!type:Html.attr}. *)

type 'a signal = 'a Signal.t
(** An alias for {!type:Signal.t}. *)

val signal : ?equal:('a -> 'a -> bool) -> ?label:string -> 'a -> 'a signal
(** An alias for {!val:Signal.make}. *)

(** {1 Reactive views} *)

val show : ?label:string -> ('a -> html) -> 'a signal -> html
(** [show to_html signal] is a dynamic HTML node created from [signal] values using [to_html]. *)

val show_some : ?label:string -> ('a -> html) -> 'a option signal -> html
(** [show_some] is similar to {!val:show}, but operates on reactive option values. When the signal's
    value is [None], {!val:Html.empty} is rendered. *)

val show_ok : ?label:string -> ('a -> html) -> ('a, _) result signal -> html
(** [show_ok] is similar to {!val:show}, but operates on reactive result values. When the signal's
    value is [Error _], {!val:Html.empty} is rendered. *)

val each : ?key:('a -> string) -> ('a -> html) -> 'a list signal -> html
(** [each to_html signal] reactively renders items from [signal] with [to_html].

    {[
      let items = Signal.make [ 1; 2; 3 ] in
      ul [] [ each (fun item -> li [] [ int item ]) items ]
    ]} *)

(** {1 Dynamic attributes} *)

val bind : ('a -> Html.attr) -> 'a Signal.t -> Html.attr
(** [bind to_attr signal] is a dynamic HTML attribute created from [signal] values using [to_attr].

    {[
      let style = Signal.make [ ("color", "red") ] in
      div [ bind Html.style style ] [ text "Hello!" ]
    ]} *)

val bind_some : ('a -> Html.attr) -> 'a option Signal.t -> Html.attr
(** [bind_some] is similar to {!val:bind}, but operates on reactive option values. When the signal's
    value is [None], an empty attribute is produced. *)

val bind_ok : ('a -> Html.attr) -> ('a, _) result Signal.t -> Html.attr
(** [bind_ok] is similar to {!val:bind}, but operates on reactive result values. When the signal's
    value is [Error _], an empty attribute is produced. *)

val toggle : on:('a -> bool) -> 'a Signal.t -> Html.attr -> Html.attr
(** [toggle ~on:pred attr s] is [attr] if [pred x] is [true] and {!val:Html.empty} otherwise, where
    [x] is the value of [s]. *)

val conditional : on:('a -> bool) -> 'a Signal.t -> Html.html -> Html.html
(** [conditional on:signal] an attribute that shows the element if [signal] is [true]. *)

val visible : on:('a -> bool) -> 'a Signal.t -> Html.attr
(** [visible ~on:signal] is a reactive attribute that controls the [display] style of HTML elements.
    When [signal] is [false] this attribute is [display: none]. *)

module Mouse : sig
  (** Mouse signals. *)

  val position : unit -> (float * float) Signal.t
end

module Time : sig
  (** Time signals. *)

  val tick : ms:int -> unit Signal.t
end

module Http : sig
  (** HTTP requests. *)

  type error =
    | Fetch_error of Jx.t  (** An error occurred during fetch request. *)
    | Unsuccessful of Stdweb.Fetch.Response.t  (** A non-200 response from the server. *)
    | Decoding_error of exn  (** Error when decoding payload. *)
    | Handling_error of exn  (** Error when handling successful response payload. *)

  val string_of_error : error -> string

  type nonrec 'a result = ('a, error) result

  (** Request body encoder *)
  module Encoder : sig
    type 'a t

    val make : ?content_type:string -> ('a -> Stdweb.Fetch.Body.t) -> 'a t
    val empty : unit t
    val text : string t
    val form_data : Stdweb.Form_data.t t
    val form_urlencoded : (string * string) list t
    val json : Stdweb.Json.t t
  end

  (** Response body decoder *)
  module Decoder : sig
    type 'a t

    val ignore : unit t
    val text : string t
    val json : Stdweb.Json.t t
    val map : ('a -> ('b, 'err) Result.t) -> 'a t -> 'b t
  end

  val request :
    ?meth:Stdweb.Fetch.meth ->
    ?headers:(string * string) list ->
    ?mode:Stdweb.Fetch.mode ->
    ?credentials:Stdweb.Fetch.credentials ->
    encode:'a Encoder.t ->
    decode:'b Decoder.t ->
    url:string ->
    'a ->
    'b result option Signal.t

  val get :
    ?headers:(string * string) list ->
    ?mode:Stdweb.Fetch.mode ->
    ?credentials:Stdweb.Fetch.credentials ->
    decode:'a Decoder.t ->
    url:string ->
    unit ->
    'a result option Signal.t

  val put :
    ?headers:(string * string) list ->
    ?mode:Stdweb.Fetch.mode ->
    ?credentials:Stdweb.Fetch.credentials ->
    encode:'a Encoder.t ->
    decode:'b Decoder.t ->
    url:string ->
    'a ->
    'b result option Signal.t

  val post :
    ?headers:(string * string) list ->
    ?mode:Stdweb.Fetch.mode ->
    ?credentials:Stdweb.Fetch.credentials ->
    encode:'a Encoder.t ->
    decode:'b Decoder.t ->
    url:string ->
    'a ->
    'b result option Signal.t

  val delete :
    ?headers:(string * string) list ->
    ?mode:Stdweb.Fetch.mode ->
    ?credentials:Stdweb.Fetch.credentials ->
    decode:'b Decoder.t ->
    url:string ->
    'a ->
    'b result option Signal.t

  module Json : sig
    val request :
      ?meth:Stdweb.Fetch.meth ->
      ?headers:(string * string) list ->
      ?mode:Stdweb.Fetch.mode ->
      ?credentials:Stdweb.Fetch.credentials ->
      encode:('a -> Stdweb.Json.t) ->
      decode:(Stdweb.Json.t -> ('b, 'err) Result.t) ->
      url:string ->
      'a ->
      'b result option Signal.t

    val get :
      ?headers:(string * string) list ->
      ?mode:Stdweb.Fetch.mode ->
      ?credentials:Stdweb.Fetch.credentials ->
      decode:(Stdweb.Json.t -> ('a, 'err) Result.t) ->
      url:string ->
      unit ->
      'a result option Signal.t

    val put :
      ?headers:(string * string) list ->
      ?mode:Stdweb.Fetch.mode ->
      ?credentials:Stdweb.Fetch.credentials ->
      encode:('a -> Stdweb.Json.t) ->
      decode:(Stdweb.Json.t -> ('b, 'err) Result.t) ->
      url:string ->
      'a ->
      'b result option Signal.t

    val post :
      ?headers:(string * string) list ->
      ?mode:Stdweb.Fetch.mode ->
      ?credentials:Stdweb.Fetch.credentials ->
      encode:('a -> Stdweb.Json.t) ->
      decode:(Stdweb.Json.t -> ('b, 'err) Result.t) ->
      url:string ->
      'a ->
      'b result option Signal.t

    val delete :
      ?headers:(string * string) list ->
      ?mode:Stdweb.Fetch.mode ->
      ?credentials:Stdweb.Fetch.credentials ->
      decode:(Stdweb.Json.t -> ('b, 'err) Result.t) ->
      url:string ->
      'a ->
      'b result option Signal.t
  end
end

module History : sig
  (** Browser location and navigation helpers. *)

  val location : Stdweb.Dom.Location.t Signal.t
  val hash : string Signal.t
  val hash_path : string list Signal.t
  val go : string -> unit
end

(** Reactive routing.

    - typed path matching
    - reactive variable updates
    - rest capture for nested routing
    - any capture for skipping segments
    - custom variable capture
    - relative path construction (sprintf)
    - path prefix checks (is_active)
    - upstream variable-to-hash update *)
module Router : sig
  type t
  (** Represents the current routing state and can be used to create links and dispatch routes to
      views. *)

  val make : ?prefix:string list signal -> string list signal -> t
  (** [make path_signal] is a router scope given the current path. *)

  type route
  (** Assigns a path to a view to be rendered on match. *)

  type 'a var
  (** Variables found in routing paths. For exmaple, ["/users/:int"] contains a int variable. *)

  val var :
    of_string:(string -> 'a option) ->
    to_string:('a -> string) ->
    ?equal:('a -> 'a -> bool) ->
    string ->
    'a var
  (** [var ~of_string ~to_string ?equal label] creates a variable. *)

  val int : int var
  (** A variable that decodes int values. *)

  val string : string var
  (** A variable that decodes string values. *)

  val query : Stdweb.Url_search_params.t var
  (** A variable that decodes
      {{:https://developer.mozilla.org/en-US/docs/Web/API/URLSearchParams/URLSearchParams}
        [query parameters]} (e.g. [name=ferret&count=purple]). *)

  (** Represents paths that can be used to (1) dispatch routing state to views, and (2) to generate
      dynamic links.

      A path is either a constatn segment, a variable or a special wildcard "rest" segment. Constant
      segments are matched verbatim, variable segments will capture a typed value from the path and
      dispatch it to the view function and, finally, rest segments represent nested router scopes.

      Note: path types with type [('a, 'a, 'a) path] represent static paths, i.e., complete paths
      without variables. *)
  type ('view, 'link, 'a) path =
    | Const : string * ('view, 'link, 'a) path -> ('view, 'link, 'a) path
    | Var :
        'v var * 'v signal option * ('view, 'link, 'a) path
        -> ('v signal -> 'view, 'v -> 'link, 'a) path
    | Rest : (t -> 'a, ('view, 'link, 'a) path -> 'link, 'a) path
    | End : (unit -> 'a, 'a, 'a) path

  val link :
    ?absolute:bool ->
    ?up:int ->
    ?active:Html.attr ->
    ?inactive:Html.attr ->
    ?exact:bool ->
    ?alias:(unit -> 'alias, 'alias, 'alias) path ->
    t ->
    ('view, 'link, Html.attr) path ->
    'link
  (** [link ?absolute ?up ?active ?exact ?alias router path vars...] is an HTML [href] attribute
      that binds a link described by [path] and any [vars] contained in it (or none, if it's a const
      only path). A link relative to [router] will be created (with the level adjusted by [up]),
      unless [absolute] is [true], in which case the [router] is ignored.

      If [active] attribute is provided, in addition to binding [href], [active] will be bound in
      case the current path is active, otherwise [inactive] is bound (if provided).

      By default, a path is considered active if it is a prefix of the current path. If [exact] is
      [true], the path is only considered active when it is equal to the current path. Additionally,
      the path is considered active if it is equal to [alias]. *)

  val route : ('view, 'link, html) path -> 'view -> route
  (** Create a route by assigning a path to a view. *)

  val alias : (unit -> 'a, 'a, 'a) path -> ('view, 'link, route) path -> 'link
  (** [alias src dst vars...] creates a route by aliasing a static [src] path to a [dst] path that
      may contain [vars]. The [dst] path is always interpreted as a relative path.

      Note: an alias route does not automatically update the location in the browser. *)

  val go : ?absolute:bool -> ?up:int -> t -> (_, 'link, unit) path -> 'link
  (** [go ?absolute ?up path vars...] navigates to [path] by updating browser's hash, which will
      trigger a routing event. *)

  val dispatch : ?label:string -> ?default:html -> t -> route list -> html
  (** [dispatch router routes] the current routing state described by [router] to [routes] rendering
      a view that matches the current path. If no matches are found, render [default]. *)

  val prefix : t -> string list signal
  (** The prefix of this, potentially nested, router. *)

  val path : t -> string list signal
  (** The current unmatched path of the router. *)

  val string_of_path : ('view, 'link, 'a) path -> string
  (** Convert a path to a string. *)
end

(** {1 Syntax}

    [let] operators are provided to simplify rendering signals to HTML: [(let$)] and [(and$)].

    Example:

    {[
      let view user_id todo_title =
        let$ user_id = Signal.map int_of_string user_id in
        and$ todo_title in
        let open Html in
        div [] [ h2 [] [ text "User id: "; text user_id ]; h3 [] [ text "Todo: "; todo_title ] ]
    ]}

    Which is the same as:

    {[
      let view user_id todo_title =
        let user_id = Signal.map int_of_string site_id in
        show
          (fun (user_id, todo_title) ->
            let open Html in
            div [] [ h2 [] [ text "User id: "; text user_id ]; h3 [] [ text "Todo: "; todo_title ] ])
          (Signal.pair user_id todo_title)
    ]}

    Additionally, reactive attributes can be bound with [let@] and [and@]. *)

val ( let$ ) : 'a signal -> ('a -> html) -> html
val ( and$ ) : 'a signal -> 'b signal -> ('a * 'b) signal
val ( let@ ) : 'a signal -> ('a -> attr) -> attr
val ( and@ ) : 'a signal -> 'b signal -> ('a * 'b) signal

(** View debugging *)

val enable_debug : bool -> unit
(** Set to [true] to activate visual debugging details.

    If enabled, all reactive elements will be annoated with rendering details. The following format
    is used: [show:{elem_count}/{elem_label?}#{update_count}], where:

    - [{elem_count}] is the sequential count assigned to each [show] element;
    - [{elem_label}] is the optional user-provided label for the [show] element;
    - [{update_count}] is the sequential count of re-renders of the same [show] element.

    Note that the [update_count] is, in practice, the number of signal emits.

    With each update, the color of the container will change. *)
