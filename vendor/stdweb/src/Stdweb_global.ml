open struct
  module E = Jx.Encode
  module D = Jx.Decode
end

let window = Jx.expr "window"
let document = Jx.expr "document"
let console = Jx.expr "console"
let navigator = Jx.expr "navigator"

let encode_uri_component =
  let f = Jx.expr "encodeURIComponent" in
  fun x -> D.string (Jx.call f [| E.string x |])
