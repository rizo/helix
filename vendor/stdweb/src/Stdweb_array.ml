open struct
  module E = Jx.Encode
  module D = Jx.Decode
end

type 'a t = Jx.t

let t = Jx.global "Array"
let to_js p = p
let of_js js = js
let make n = Jx.obj_new t [| E.int n |]
let empty () = make 0
let set arr (i : int) x = Jx.arr_set arr i (E.any x)

let init n f =
  let out = make n in
  for i = 0 to n - 1 do
    set out i (f i)
  done;
  out

let get arr i = D.any (Jx.arr_get arr i)

let get_opt arr i =
  let x = Jx.arr_get arr i in
  if Jx.strict_equal x Jx.undefined then raise Not_found else D.any x

let push arr x = ignore (Jx.obj_call arr "push" [| E.any x |])
let pop arr = D.any (Jx.obj_call arr "pop" [||])

let pop_opt arr =
  let x = Jx.obj_call arr "pop" [||] in
  if Jx.strict_equal x Jx.undefined then None else Some (D.any x)

let length arr = D.int (Jx.obj_get arr "length")
let concat arr1 arr2 = Jx.obj_call arr1 "concat" [| arr2 |]
let slice arr i j = Jx.obj_call arr "slice" [| E.int i; E.int j |]
let iter arr f = ignore (Jx.obj_call arr "forEach" [| E.func 1 f |])

let of_list l =
  match l with
  | [] -> empty ()
  | hd :: tl ->
    let out = make 1 in
    set out 0 hd;
    List.iteri (fun i x -> set out (i + 1) x) tl;
    out
