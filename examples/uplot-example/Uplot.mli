module Element := Stdweb.Dom.Element
module Html := Helix.Html
module Js := Helix.Js

type t

val data_to_js : float array array Js.encoder
val make : options:Js.Obj.t -> data:float array array -> Element.t -> t

val mount :
  options:Js.Obj.t -> data:float array array -> t option ref -> Html.attr

val set_data : t -> float array array -> unit
