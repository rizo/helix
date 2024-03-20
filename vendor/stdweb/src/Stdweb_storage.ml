type t = Jx.t

let local = Jx.global "localStorage"

let get t name =
  Jx.Obj.call1 t "getItem"
    ~return:Jx.Decoder.(nullable string)
    Jx.Encoder.string name

let set t name value =
  Jx.Obj.call2_unit t "setItem" Jx.Encoder.string Jx.Encoder.string name value

let del t name = Jx.Obj.call1_unit t "removeItem" Jx.Encoder.string name
