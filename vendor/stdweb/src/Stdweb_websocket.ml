open struct
  module Global = Stdweb_global
  module Event = Stdweb_dom.Event
end

type t = Jx.t

module Event = struct
  let data t = Jx.Obj.get t "data" Jx.Decoder.string

  (* Generic events *)

  let open' = Event.Name.make "open"
  let message = Event.Name.make "message"
  let close = Event.Name.make "close"
  let error = Event.Name.make "error"
end

let t = Jx.global "WebSocket"
let make url = Jx.Obj.new1 t Jx.Encoder.string url

let bind this event_type f =
  Jx.Obj.call2_unit this "addEventListener" Jx.Encoder.js Jx.Encoder.fun1
    event_type f

let unbind this event_type =
  Jx.Obj.call_js_unit this "removeEventLister" [| Jx.Encoder.js event_type |]

(* TODO: parameters `code` and `reason` not implemented yet *)
let close ?code ?reason t =
  Jx.Obj.call2_unit t "close"
    (Jx.Encoder.optional Jx.Encoder.int)
    (Jx.Encoder.optional Jx.Encoder.string)
    code reason

let send t data = Jx.Obj.call1_unit t "send" Jx.Encoder.string data
