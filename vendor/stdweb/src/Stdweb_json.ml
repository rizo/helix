type t = Jx.t

let t = Jx.global "JSON"

let stringify ?(replacer = Jx.undefined) ?(space = Jx.undefined) value =
  Jx.Decode.string
    (Jx.obj_call t "stringify" [| Jx.Encode.any value; replacer; space |])

let parse ?reviver text =
  let reviver =
    match reviver with
    | None -> Jx.undefined
    | Some reviver -> reviver
  in
  Jx.obj_call t "parse" [| Jx.Encode.string text; reviver |]
