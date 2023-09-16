type 'a t = Jx.t
type 'a next = Jx.t

let next t = Jx.Obj.call_js t "next" [||]
let next_is_done next = Jx.Obj.get next "done" Jx.Decoder.bool
let next_value next = Jx.Obj.get next "value" Jx.Decoder.any

let iter f t =
  let is_done = ref false in
  while not !is_done do
    let n = next t in
    if next_is_done n then is_done := true
    else
      let v = next_value n in
      f v
  done
