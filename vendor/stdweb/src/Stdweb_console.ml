module Global = Stdweb_global

type t = Jx.t

let t = Global.console
let log x = ignore (Jx.obj_call t "log" [| Jx.Encode.any x |])
let error x = ignore (Jx.obj_call t "error" [| Jx.Encode.any x |])
let info x = ignore (Jx.obj_call t "info" [| Jx.Encode.any x |])
let warn x = ignore (Jx.obj_call t "warn" [| Jx.Encode.any x |])

let ensure b x =
  ignore (Jx.obj_call t "assert" [| Jx.Encode.bool b; Jx.Encode.any x |])
