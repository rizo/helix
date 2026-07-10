type 'a t = Jx.t
type 'a next = Jx.t

let next t = Jx.obj_call t "next" [||]
let next_is_done next = Jx.Decode.bool (Jx.obj_get next "done")
let next_value next = Jx.Decode.any (Jx.obj_get next "value")

let iter f t =
  let is_done = ref false in
  while not !is_done do
    let n = next t in
    if next_is_done n then is_done := true
    else
      let v = next_value n in
      f v
  done

let unsafe_of_js js = js
