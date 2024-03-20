open struct
  module Global = Stdweb_global
end

type t = Jx.t

let t = Jx.global "URLSearchParams"
let of_string s = Jx.Obj.new1 t Jx.Encoder.string s
let empty () = Jx.Obj.new0 t

let make items =
  Jx.Obj.new1 t Jx.Encoder.obj
    (List.map (fun (k, v) -> (k, Jx.Encoder.string v)) items)

let get t param =
  Jx.Obj.call1 t "get"
    ~return:Jx.Decoder.(nullable string)
    Jx.Encoder.string param

let delete t ?value param =
  let value =
    match value with
    | None -> Jx.undefined
    | Some value -> Jx.encode Jx.Encoder.string value
  in
  Jx.Obj.call2_unit t "delete" Jx.Encoder.string Jx.Encoder.any param value

let has t ?value text =
  let value =
    match value with
    | None -> Jx.undefined
    | Some value -> Jx.encode Jx.Encoder.string value
  in
  Jx.Obj.call2 t "has" ~return:Jx.Decoder.bool Jx.Encoder.string Jx.Encoder.any
    text value

let to_string t = Jx.Obj.call0 t "toString" ~return:Jx.Decoder.string ()
