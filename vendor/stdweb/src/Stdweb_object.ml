type t = Jx.t

let t = Jx.global "Object"
let from_entries entries = Jx.obj_call t "fromEntries" [| entries |]
