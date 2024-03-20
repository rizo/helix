let window = Jx.global "window"
let document = Jx.global "document"
let console = Jx.global "console"
let navigator = Jx.global "navigator"

let encode_uri_component =
  let f = Jx.global "encodeURIComponent" in
  Jx.Fun.call1 f ~return:Jx.Decoder.string Jx.Encoder.string
