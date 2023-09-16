module Stdlib_obj = Obj
module Stdlib_array = Array

type 'a t = Jx.Obj.t

let to_js p = p
let of_js js = js
let to_obj p = p
let of_obj obj = obj
let empty = Jx.Obj.empty

let of_array arr =
  let arr =
    (Stdlib_obj.magic : (string * 'a) array -> (string * Jx.t) array) arr
  in
  Jx.Obj.of_array arr

let of_list l = of_array (Stdlib_array.of_list l)
let get dict key = Jx.Obj.get dict key Jx.Decoder.any
let get_opt dict key = Jx.Obj.get_opt dict key Jx.Decoder.any
let set dict key x = Jx.Obj.set dict key Jx.Encoder.any x
let del = Jx.Obj.del

let entry_of_js entry_js =
  match Jx.Decoder.array_js entry_js with
  | [| key; v |] -> (Jx.Decoder.string key, Jx.Decoder.any v)
  | _ -> invalid_arg "Object entries is not a pair"

let entries dict =
  Jx.Obj.call1 Jx.Obj.t "entries"
    ~return:(Jx.Decoder.array entry_of_js)
    Jx.Encoder.js dict

let keys dict =
  Jx.Obj.call1 Jx.Obj.t "keys"
    ~return:Jx.Decoder.(array string)
    Jx.Encoder.js dict

let values dict =
  Jx.Obj.call1 Jx.Obj.t "values"
    ~return:Jx.Decoder.(array any)
    Jx.Encoder.js dict

let map dict f =
  let out = empty () in
  let keys = keys dict in
  for i = 0 to Stdlib_array.length keys - 1 do
    let key = Stdlib_array.unsafe_get keys i in
    let x = get dict key in
    let x' = f x in
    set out key x'
  done;
  out

let update dict f =
  let keys = keys dict in
  for i = 0 to Stdlib_array.length keys - 1 do
    let key = Stdlib_array.unsafe_get keys i in
    let x = get dict key in
    let x' = f x in
    set dict key x'
  done

let fold_left dict f init =
  let acc = ref init in
  let values = values dict in
  for i = 0 to Stdlib_array.length values - 1 do
    let x = Stdlib_array.unsafe_get values i in
    acc := f !acc x
  done;
  !acc

let iter dict f =
  let values = values dict in
  for i = 0 to Stdlib_array.length values - 1 do
    let x = Stdlib_array.unsafe_get values i in
    f x
  done
