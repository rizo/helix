open struct
  module Window = Stdweb.Dom.Window
  module Location = Stdweb.Dom.Location
  module Event = Stdweb.Dom.Event
end

(* TODO: make these signals lazy. *)

let location =
  let s = Signal.make Window.location in
  Window.bind Event.popstate (fun _ -> Signal.emit Window.location s);
  s

let hash =
  let s = Signal.make (Location.hash Window.location) in
  Window.bind Event.hashchange (fun _ ->
      let hash = Location.hash Window.location in
      Signal.emit hash s
  );
  s

let hash_to_path str =
  match String.split_on_char '/' str with
  | [] -> []
  | [ "" ] -> []
  | "#" :: tl -> tl
  | _ :: _ -> invalid_arg str

let hash_path = Signal.map hash_to_path hash
let go href = Location.set_href Window.location href
