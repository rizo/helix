module Promise = Stdweb_promise

type t = Jx.t

let write_text t text =
  Jx.Obj.call1 t "writeText" ~return:Promise.of_js Jx.Encoder.string text
