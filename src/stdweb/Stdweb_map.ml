module Iterator = Stdweb_iterator

type 'a t = Jx.t

let t = Jx.global "Map"
let to_js t = t
let of_js t = t
let make () = Jx.Obj.new0 t
let clear t = Jx.Obj.call_js_unit t "clear" [||]
let set t k v = Jx.Obj.call_js_unit t "set" [| k; Jx.Encoder.any v |]
let get t k = Jx.Decoder.any (Jx.Obj.call_js t "get" [| k |])
let delete t k = Jx.Obj.call_js_unit t "delete" [| k |]
let keys t = Jx.Obj.call_js t "keys" [||]
let size t = Jx.Obj.get t "size" Jx.Decoder.int
let values t = Jx.Obj.call_js t "values" [||]

let first_key t =
  let iter = keys t in
  let next = Iterator.next iter in
  if Iterator.next_is_done next then None else Some (Iterator.next_value next)
