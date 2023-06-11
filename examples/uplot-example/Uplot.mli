module Element := Stdweb.Dom.Element
module Html := Helix.Html

type t

val data_to_js : float array array Jx.encoder
val make : options:Jx.Obj.t -> data:float array array -> Element.t -> t

val mount :
  options:Jx.Obj.t -> data:float array array -> t option ref -> Html.attr

val set_data : t -> float array array -> unit
