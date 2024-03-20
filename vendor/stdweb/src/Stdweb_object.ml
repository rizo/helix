type t = Jx.t

let t = Jx.global "Object"

let from_entries =
  Jx.Obj.call1 t "fromEntries" ~return:Jx.Decoder.js Jx.Encoder.js
