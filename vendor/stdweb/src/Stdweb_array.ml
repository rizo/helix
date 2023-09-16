type 'a t = Jx.t

let t = Jx.global "Array"
let to_js p = p
let of_js js = js
let make n = Jx.Obj.new1 t Jx.Encoder.int n
let empty () = make 0
let set arr (i : int) x = Jx.Obj.set_js arr (Jx.Encoder.int i) (Jx.Encoder.any x)

let init n f =
  let out = make n in
  for i = 0 to n - 1 do
    set out i (f i)
  done;
  out

let get arr i = Jx.Decoder.any (Jx.Obj.get_js arr (Jx.Encoder.int i))

let get_opt arr i =
  let x = get arr i in
  let x_js = Jx.Encoder.any x in
  if Jx.is_undefined x_js then raise Not_found else x

let push arr x = Jx.Obj.call1_unit arr "push" Jx.Encoder.any x
let pop arr = Jx.Obj.call0 arr "pop" ~return:Jx.Decoder.any ()
let pop_opt arr = Jx.Obj.call0 arr "pop" ~return:Jx.Decoder.(optional any) ()
let length arr = Jx.Obj.get arr "length" Jx.Decoder.int
let iter arr f = Jx.Obj.call1_unit arr "forEach" Jx.Encoder.fun1 f

let of_list l =
  match l with
  | [] -> empty ()
  | hd :: tl ->
    let out = make 1 in
    set out 0 hd;
    List.iteri (fun i x -> set out (i + 1) x) tl;
    out
