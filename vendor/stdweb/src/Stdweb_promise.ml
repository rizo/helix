type 'a t = Jx.t
type ('a, 'err) executor = ('a -> unit) -> ('err -> unit) -> unit

let to_js p = p
let of_js js = js
let t = Jx.global "Promise"
let make executor = Jx.obj_new t [| Jx.Encode.func 2 executor |]
let resolve v = Jx.obj_call t "resolve" [| Jx.Encode.any v |]
let reject err = Jx.obj_call t "reject" [| Jx.Encode.any err |]
let and_then f p = Jx.obj_call p "then" [| Jx.Encode.func 1 f |]
let map f p = and_then (fun x -> resolve (f x)) p
let use f p = ignore (Jx.obj_call p "then" [| Jx.Encode.func 1 f |])
let catch f p = ignore (Jx.obj_call p "catch" [| Jx.Encode.func 1 f |])
let ignore p = use ignore p

module Syntax = struct
  let ( let* ) p f = and_then f p
end
