module Element = Stdweb.Dom.Element
module Js = Helix.Js

type t

val data_to_js : float array array Js.encoder
val make : options:Js.Obj.t -> data:float array array -> Element.t -> t
val set_data : t -> float array array -> unit
