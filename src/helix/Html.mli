(** Build HTML content. *)

open Stdweb

type node = Dom.Node.t

type html
(** The type for HTML elements or character data. *)

(** {1:attr Attributes}

    See the {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes} MDN
    HTML attribute reference}. *)

type attr
(** The type for attributes and their values. *)

val attr : string -> string -> attr
(** [attr name v] is an attribute [name] with value [v]. *)

val accept : string -> attr
(** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/accept}
    accept}. *)

val accesskey : string -> attr
(** See
    {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/accesskey}
    accesskey}. *)

val action : string -> attr
(** See
    {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/form#attr-action}
    action}. *)

val autocomplete : string -> attr
(** See
    {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/autocomplete}
    autocomplete}. *)

val autofocus : attr
(** See
    {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/autofocus}
    autofocus}. *)

val charset : string -> attr
(** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes}
    charset}. *)

val checked : attr
(** See
    {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input#checked}
    checked}. *)

val class_name : string -> attr
(** See
    {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/class}
    class}. *)

val cols : int -> attr
(** See
    {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/textarea#attr-cols}
    cols}. *)

val content : string -> attr
(** See
    {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/meta#attr-content}
    content}. *)

val contenteditable : bool -> attr
(** See
    {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/contenteditable}
    contenteditable}. *)

val defer : attr
(** See
    {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/script#attr-defer}
    defer}. *)

val dir : string -> attr
(** See
    {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/dir}
    dir}. *)

val disabled : attr
(** See
    {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/disabled}
    disabled}. *)

val draggable : bool -> attr
(** See
    {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/draggable}
    draggable}. *)

val for' : string -> attr
(** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/for}
    for'}. *)

val height : int -> attr
(** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes}
    height}. *)

val hidden : attr
(** See
    {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/hidden}
    hidden}. *)

val href : string -> attr
(** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes} href}. *)

val id : string -> attr
(** See
    {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/id}
    id}. *)

val lang : string -> attr
(** See
    {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/lang}
    lang}. *)

val list : string -> attr
(** See
    {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input#attr-list}
    list}. *)

val media : string -> attr
(** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes} media}. *)

val method' : string -> attr
(** See
    {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/form#attr-method}
    method}. *)

val name : string -> attr
(** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes} name}. *)

val placeholder : string -> attr
(** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes}
    placeholder}. *)

val rel : string -> attr
(** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/rel}
    rel}. *)

val required : attr
(** See
    {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/required}
    required}. *)

val rows : int -> attr
(** See
    {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/textarea#attr-rows}
    rows}. *)

val selected : attr
(** See
    {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/option#attr-selected}
    selected}. *)

val spellcheck : string -> attr
(** See
    {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/spellcheck}
    spellcheck}. *)

val src : string -> attr
(** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes} src}. *)

val style : (string * string) list -> attr
(** See
    {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/style}
    style}. *)

val tabindex : int -> attr
(** See
    {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/tabindex}
    tabindex}. *)

val title : string -> attr
(** See
    {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/title}
    title}. *)

val type' : string -> attr
(** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes} type}. *)

val value : string -> attr
(** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes} value}. *)

val wrap : string -> attr
(** See
    {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/textarea#attr-wrap}
    wrap}. *)

val width : int -> attr
(** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes} width}. *)

val class_list : string list -> attr
(** [class_list list] is similar to {!class_name} but accepts a list of class
    names. *)

val class_flags : (string * bool) list -> attr
(** [class_flags list] is similar to {!class_list}, but can conditionally omit
    class names depending on the boolean values in [list]. *)

(** {2 Event attributes} *)

val on : 'a Dom.Event.kind -> ('a Dom.Event.t -> unit) -> attr
val on_click : (Dom.Event.Mouse.t -> unit) -> attr
val on_input : (Dom.Event.Input.t -> unit) -> attr
val on_keydown : (Dom.Event.Keyboard.t -> unit) -> attr

(** Additional attribute operations. *)
module Attr : sig
  type t = attr
  (** The type for HTML attributes. *)

  val empty : attr
  (** [empty] is an attribute that doesn't get rendered. *)

  val bool : string -> bool -> attr
  (** [bool name value] is [attr name ""] if [value] is [true] and {!empty}
      otherwise.

      This sets the
      {{:https://html.spec.whatwg.org/multipage/common-microsyntaxes.html#boolean-attributes}
      boolean attribute} [n] to true. The attribute will be omitted if [b] is
      false. *)

  val int : string -> int -> attr
  (** [int name value] is [attr name (string_of_int i)]. *)

  val on : bool -> t -> attr
  (** [on cond attr] is [attr] if [cond] is [true] and {!empty} otherwise. *)

  val on_some : attr option -> attr
  (** [on_some option] is [attr] if [option] is [Some attr] and {!empty} if
      [option] is [None]. *)

  val on_ok : (attr, 'e) result -> attr
  (** [on_ok result] is [attr] if [result] is [Ok attr] and {!empty} if [result]
      is [Error _]. *)

  val on_mount : (Dom.Element.t -> unit) -> attr
  (** [on_mount f] is an HTML attribute that calls [f] with an element this
      attribute is added to. *)

  module Internal : sig
    type t = { set : Dom.Element.t -> unit; remove : Dom.Element.t -> unit }

    val of_attr : attr -> t
    val to_attr : t -> attr
  end
end

(** {1:elem Elements} *)

val node : string -> attr list -> node list -> node

val elem : string -> attr list -> html list -> html
(** [elem name attrs children] is an HTML element named [name] with attributes
    [attr] and [children]. *)

val empty : html
(** [empty] is an empty element that will not be rendered. *)

val text : string -> html
(** [text s] is character data [s]. [s] will be escaped. *)

val int : int -> html
(** [int n] is [text (string_of_int n)]. *)

val nbsp : html
(** [nbsp] is [text "\u{00A0}"]. *)

val fragment : html list -> html
(** See {{:https://developer.mozilla.org/en-US/docs/Web/API/DocumentFragment}
    [DocumentFragment]}. *)

val a : attr list -> html list -> html
(** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/a} a}. *)

val abbr : attr list -> html list -> html
(** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/abbr}
    abbr}. *)

val address : attr list -> html list -> html
(** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/address}
    address}. *)

val area : attr list -> html
(** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/area}
    area}. *)

val article : attr list -> html list -> html
(** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/article}
    article}. *)

val aside : attr list -> html list -> html
(** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/aside}
    aside}. *)

val audio : attr list -> html list -> html
(** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/audio}
    audio}. *)

val b : attr list -> html list -> html
(** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/b} b}. *)

val base : attr list -> html
(** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/base}
    base}. *)

val bdi : attr list -> html list -> html
(** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/bdi} bdi}. *)

val bdo : attr list -> html list -> html
(** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/bdo} bdo}. *)

val blockquote : attr list -> html list -> html
(** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/blockquote}
    blockquote}. *)

val br : attr list -> html
(** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/br} br}. *)

val button : attr list -> html list -> html
(** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/button}
    button}. *)

val canvas : attr list -> html list -> html
(** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/canvas}
    canvas}. *)

val caption : attr list -> html list -> html
(** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/caption}
    caption}. *)

val cite : attr list -> html list -> html
(** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/cite}
    cite}. *)

val code : attr list -> string -> html
(** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/code}
    code}. *)

val col : attr list -> html
(** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/col} col}. *)

val colgroup : attr list -> html list -> html
(** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/colgroup}
    colgroup}. *)

val command : attr list -> html list -> html
(** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/command}
    command}. *)

val datalist : attr list -> html list -> html
(** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/datalist}
    datalist}. *)

val dd : attr list -> html list -> html
(** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dd} dd}. *)

val del : attr list -> html list -> html
(** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/del} del}. *)

val details : attr list -> html list -> html
(** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/details}
    details}. *)

val dfn : attr list -> html list -> html
(** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dfn} dfn}. *)

val div : attr list -> html list -> html
(** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/div} div}. *)

val dl : attr list -> html list -> html
(** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dl} dl}. *)

val dt : attr list -> html list -> html
(** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dt} dt}. *)

val em : attr list -> html list -> html
(** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/em} em}. *)

val embed : attr list -> html
(** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/embed}
    embed}. *)

val fieldset : attr list -> html list -> html
(** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/fieldset}
    fieldset}. *)

val figcaption : attr list -> html list -> html
(** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/figcaption}
    figcaption}. *)

val figure : attr list -> html list -> html
(** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/figure}
    figure}. *)

val footer : attr list -> html list -> html
(** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/footer}
    footer}. *)

val form : attr list -> html list -> html
(** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/form}
    form}. *)

val h1 : attr list -> html list -> html
(** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h1} h1}. *)

val h2 : attr list -> html list -> html
(** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h2} h2}. *)

val h3 : attr list -> html list -> html
(** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h3} h3}. *)

val h4 : attr list -> html list -> html
(** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h4} h4}. *)

val h5 : attr list -> html list -> html
(** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h5} h5}. *)

val h6 : attr list -> html list -> html
(** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h6} h6}. *)

val head : attr list -> html list -> html
(** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/head}
    head}. *)

val header : attr list -> html list -> html
(** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/header}
    header}. *)

val hgroup : attr list -> html list -> html
(** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/hgroup}
    hgroup}. *)

val hr : attr list -> html
(** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/hr} hr}. *)

val html : attr list -> html list -> html
(** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/html}
    html}. *)

val i : attr list -> html list -> html
(** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/i} i}. *)

val iframe : attr list -> html list -> html
(** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/iframe}
    iframe}. *)

val img : attr list -> html
(** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/img} img}. *)

val input : attr list -> html
(** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input}
    input}. *)

val ins : attr list -> html list -> html
(** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/ins} ins}. *)

val kbd : attr list -> html list -> html
(** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/kbd} kbd}. *)

val keygen : attr list -> html list -> html
(** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/keygen}
    keygen}. *)

val label : attr list -> html list -> html
(** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/label}
    label}. *)

val legend : attr list -> html list -> html
(** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/legend}
    legend}. *)

val li : attr list -> html list -> html
(** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/li} li}. *)

val main : attr list -> html list -> html
(** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/main}
    main}. *)

val map : attr list -> html list -> html
(** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/map} map}. *)

val mark : attr list -> html list -> html
(** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/mark}
    mark}. *)

val menu : attr list -> html list -> html
(** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/menu}
    menu}. *)

val meta : attr list -> html
(** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/meta}
    meta}. *)

val meter : attr list -> html list -> html
(** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/meter}
    meter}. *)

val nav : attr list -> html list -> html
(** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/nav} nav}. *)

val object' : attr list -> html list -> html
(** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/object}
    object}. *)

val ol : attr list -> html list -> html
(** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/ol} ol}. *)

val optgroup : attr list -> html list -> html
(** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/optgroup}
    optgroup}. *)

val option : attr list -> html list -> html
(** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/option}
    option}. *)

val output : attr list -> html list -> html
(** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/output}
    output}. *)

val p : attr list -> html list -> html
(** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/p} p}. *)

val param : attr list -> html
(** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/param}
    param}. *)

val pre : attr list -> html list -> html
(** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/pre} pre}. *)

val progress : attr list -> html list -> html
(** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/progress}
    progress}. *)

val q : attr list -> html list -> html
(** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/q} q}. *)

val rp : attr list -> html list -> html
(** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/rp} rp}. *)

val rt : attr list -> html list -> html
(** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/rt} rt}. *)

val ruby : attr list -> html list -> html
(** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/ruby}
    ruby}. *)

val s : attr list -> html list -> html
(** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/s} s}. *)

val samp : attr list -> html list -> html
(** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/samp}
    samp}. *)

val section : attr list -> html list -> html
(** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/section}
    section}. *)

val select : attr list -> html list -> html
(** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/select}
    select}. *)

val small : attr list -> html list -> html
(** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/small}
    small}. *)

val source : attr list -> html
(** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/source}
    source}. *)

val span : attr list -> html list -> html
(** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/span}
    span}. *)

val strong : attr list -> html list -> html
(** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/strong}
    strong}. *)

val sub : attr list -> html list -> html
(** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/sub} sub}. *)

val summary : attr list -> html list -> html
(** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/summary}
    summary}. *)

val sup : attr list -> html list -> html
(** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/sup} sup}. *)

val table : attr list -> html list -> html
(** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/table}
    table}. *)

val tbody : attr list -> html list -> html
(** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/tbody}
    tbody}. *)

val td : attr list -> html list -> html
(** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/td} td}. *)

val textarea : attr list -> html list -> html
(** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/textarea}
    textarea}. *)

val tfoot : attr list -> html list -> html
(** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/tfoot}
    tfoot}. *)

val th : attr list -> html list -> html
(** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/th} th}. *)

val thead : attr list -> html list -> html
(** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/thead}
    thead}. *)

val time : attr list -> html list -> html
(** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/time}
    time}. *)

val tr : attr list -> html list -> html
(** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/tr} tr}. *)

val track : attr list -> html
(** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/track}
    track}. *)

val u : attr list -> html list -> html
(** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/u} u}. *)

val ul : attr list -> html list -> html
(** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/ul} ul}. *)

val var : attr list -> html list -> html
(** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/var} var}. *)

val video : attr list -> html list -> html
(** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/video}
    video}. *)

val wbr : attr list -> html
(** See {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/wbr} wbr}. *)

(** Additional element operations. *)
module Elem : sig
  type t = html
  (** Type alias for HTML elements. *)

  val of_some : ('a -> html) -> 'a option -> html
  (** [of_some to_html option] is [to_html x] if [option] is [Some x] and
      [empty] otherwise. *)

  val of_ok : ('a -> html) -> ('a, 'e) result -> html
  (** [of_ok to_html result] is [to_html x] if [result] is [Ok x] and [empty]
      otherwise. *)

  module Internal : sig
    type t = { mount : Dom.Node.t -> unit; remove : unit -> unit }

    val of_html : html -> t
    val to_html : t -> html
  end
end

(** {2 DOM helpers} *)

val render : Dom.Element.t -> html -> unit
