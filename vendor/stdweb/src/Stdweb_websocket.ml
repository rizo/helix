open struct
  module Global = Stdweb_global
  module Event = Stdweb_dom.Event
  module E = Jx.Encode
  module D = Jx.Decode
end

type t = Jx.t

module Event = struct
  let data t = D.string (Jx.obj_get t "data")

  (* Generic events *)

  let open' = Event.Name.make "open"
  let message = Event.Name.make "message"
  let close = Event.Name.make "close"
  let error = Event.Name.make "error"
end

let t = Jx.global "WebSocket"
let make url = Jx.obj_new t [| E.string url |]

let bind this event_type f =
  ignore (Jx.obj_call this "addEventListener" [| event_type; E.func 1 f |])

let unbind this event_type =
  ignore (Jx.obj_call this "removeEventLister" [| event_type |])

(* TODO: parameters `code` and `reason` not implemented yet *)
let close ?code ?reason t =
  ignore
    (Jx.obj_call t "close"
       [|
         Stdweb_jx.encode_optional E.int code;
         Stdweb_jx.encode_optional E.string reason;
       |]
    )

let send t data = ignore (Jx.obj_call t "send" [| E.string data |])
