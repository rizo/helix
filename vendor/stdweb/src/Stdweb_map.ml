module Iterator = Stdweb_iterator

type 'a t = Jx.t

let t = Jx.global "Map"
let to_js t = t
let of_js t = t
let make () = Jx.obj_new t [||]
let clear t = ignore (Jx.obj_call t "clear" [||])
let set t k v = ignore (Jx.obj_call t "set" [| k; Jx.Encode.any v |])
let get t k = Stdweb_jx.optional Jx.Decode.any (Jx.obj_call t "get" [| k |])
let delete t k = ignore (Jx.obj_call t "delete" [| k |])
let keys t = Jx.obj_call t "keys" [||]
let size t = Jx.Decode.int (Jx.obj_get t "size")
let values t = Jx.obj_call t "values" [||]

let first_key t =
  let iter = keys t in
  let next = Iterator.next iter in
  if Iterator.next_is_done next then None else Some (Iterator.next_value next)
