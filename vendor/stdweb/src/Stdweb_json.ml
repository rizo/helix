type t = Jx.t

let t = Jx.global "JSON"

let stringify ?(replacer = Jx.undefined) ?(space = Jx.undefined) value =
  Jx.Obj.call3 t "stringify" ~return:Jx.Decoder.string Jx.Encoder.any
    Jx.Encoder.any Jx.Encoder.any value replacer space

let parse ?reviver text =
  let reviver =
    match reviver with
    | None -> Jx.undefined
    | Some reviver -> reviver
  in
  Jx.Obj.call2 t "parse" ~return:Jx.Decoder.js Jx.Encoder.string Jx.Encoder.any
    text reviver
