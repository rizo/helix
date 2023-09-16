type t

val data_to_js : float array array Jx.encoder
val make : options:Jx.Obj.t -> data:float array array -> Stdweb.Dom.node -> t

val mount :
  options:Jx.Obj.t -> data:float array array -> t option ref -> Helix.Html.attr

val set_data : t -> float array array -> unit
