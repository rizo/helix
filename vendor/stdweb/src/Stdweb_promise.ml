type 'a t = Jx.t
type ('a, 'err) executor = ('a -> unit) -> ('err -> unit) -> unit

let to_js p = p
let of_js js = js
let t = Jx.global "Promise"
let make executor = Jx.Obj.new1 t Jx.Encoder.fun1 executor
let resolve v = Jx.Obj.call1 t "resolve" ~return:of_js Jx.Encoder.any v
let reject err = Jx.Obj.call1 t "reject" ~return:of_js Jx.Encoder.any err
let and_then f p = Jx.Obj.call1 p "then" ~return:of_js Jx.Encoder.fun1 f
let map f p = and_then (fun x -> resolve (f x)) p
let use f p = Jx.Obj.call1_unit p "then" Jx.Encoder.fun1 f
let catch f p = Jx.Obj.call1_unit p "catch" Jx.Encoder.fun1 f
let ignore p = use ignore p

module Syntax = struct
  let ( let* ) p f = and_then f p
end
