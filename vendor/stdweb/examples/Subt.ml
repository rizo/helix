module type S1 = sig
  type 'target event

  module type Event_target = sig
    type t

    val add_event_listener : t -> string -> (t event -> unit) -> unit
    val remove_event_listener : t -> string -> unit
  end
end
