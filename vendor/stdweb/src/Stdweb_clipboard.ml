module Promise = Stdweb_promise

type t = Jx.t

let write_text t text = Jx.obj_call t "writeText" [| Jx.Encode.string text |]
