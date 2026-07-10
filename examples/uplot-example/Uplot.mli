type t

val data_to_js : float array array -> Jx.t
val make : options:Jx.t -> data:float array array -> Stdweb.Dom.node -> t
val mount : options:Jx.t -> data:float array array -> t option ref -> Html.attr
val set_data : t -> float array array -> unit
val set_size : w:int -> h:int -> t -> unit
