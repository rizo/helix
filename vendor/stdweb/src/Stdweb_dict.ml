module Stdlib_array = Array

open struct
  module E = Jx.Encode
  module D = Jx.Decode
end

type 'a t = Jx.t

let to_js p = p
let of_js js = js
let to_obj p = p
let of_obj obj = obj
let object_constructor = Jx.global "Object"
let empty () = E.obj [||]

let of_array arr =
  Stdlib_array.map (fun (key, value) -> (key, E.any value)) arr |> E.obj

let of_list l = of_array (Stdlib_array.of_list l)
let get dict key = D.any (Jx.obj_get dict key)
let get_opt dict key = Stdweb_jx.optional D.any (Jx.obj_get dict key)
let set dict key x = Jx.obj_set dict key (E.any x)
let del = Jx.obj_delete

let entry_of_js entry_js =
  match (D.array entry_js : Jx.t array) with
  | [| key; v |] -> (D.string key, D.any v)
  | _ -> invalid_arg "Object entries is not a pair"

let entries dict =
  let entries = Jx.obj_call object_constructor "entries" [| dict |] in
  Stdlib_array.map entry_of_js (D.array entries : Jx.t array)

let keys dict = Jx.obj_call object_constructor "keys" [| dict |] |> D.array

let values dict =
  let values = Jx.obj_call object_constructor "values" [| dict |] in
  Stdlib_array.map D.any (D.array values : Jx.t array)

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
