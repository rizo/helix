open struct
  module E = Jx.Encode
end

let window = Jx.expr "window"
let document = Jx.expr "document"
let console = Jx.expr "console"
let navigator = Jx.expr "navigator"

let encode_uri_component =
  let f = Jx.expr "encodeURIComponent" in
  fun x -> Jx.call f [| E.string x |]
