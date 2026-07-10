open struct
  module Global = Stdweb_global
  module E = Jx.Encode
  module D = Jx.Decode
end

type t = Jx.t

let t = Jx.global "URLSearchParams"
let of_string s = Jx.obj_new t [| E.string s |]
let empty () = Jx.obj_new t [||]

let make items =
  let js_items =
    List.map (fun (k, v) -> (k, E.string v)) items |> Array.of_list
  in
  Jx.obj_new t [| E.obj js_items |]

let get t param =
  Stdweb_jx.nullable D.string (Jx.obj_call t "get" [| E.string param |])

let delete t ?value param =
  let value = Stdweb_jx.encode_optional E.string value in
  ignore (Jx.obj_call t "delete" [| E.string param; value |])

let has t ?value text =
  let value = Stdweb_jx.encode_optional E.string value in
  D.bool (Jx.obj_call t "has" [| E.string text; value |])

let to_string t = D.string (Jx.obj_call t "toString" [||])
