type t = Jx.t

let local = Jx.global "localStorage"

let get t name =
  Stdweb_jx.nullable Jx.Decode.string
    (Jx.obj_call t "getItem" [| Jx.Encode.string name |])

let set t name value =
  ignore
    (Jx.obj_call t "setItem" [| Jx.Encode.string name; Jx.Encode.string value |])

let del t name = ignore (Jx.obj_call t "removeItem" [| Jx.Encode.string name |])
