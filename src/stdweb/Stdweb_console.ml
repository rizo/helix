module Global = Stdweb_global

let t = Global.console
let log x = Jx.Obj.call1_unit t "log" Jx.Encoder.any x
let error x = Jx.Obj.call1_unit t "error" Jx.Encoder.any x
let info x = Jx.Obj.call1_unit t "info" Jx.Encoder.any x
let warn x = Jx.Obj.call1_unit t "warn" Jx.Encoder.any x
let ensure b x = Jx.Obj.call2_unit t "assert" Jx.Encoder.bool Jx.Encoder.any b x
