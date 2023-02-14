(* External *)

module Stdlib_array = Array
module Stdlib_obj = Obj

type t = Helix_js_external.t
type js = t

let global_this = Helix_js_external.global_this
let null = Helix_js_external.null
let undefined = Helix_js_external.undefined
let equal = Helix_js_external.equal
let debugger = Helix_js_external.debugger
let type_of x = Helix_js_external.to_string (Helix_js_external.typeof x)
let instance_of x ~constructor = Helix_js_external.instanceof x constructor
let is_null v = v == null
let is_undefined v = v == undefined

exception Undefined_property of string

external of_any : 'a -> Helix_js_external.t = "%identity"
external to_any : Helix_js_external.t -> 'a = "%identity"

let global k =
  let x = Helix_js_external.obj_get global_this (of_any k) in
  if is_undefined x then raise (Undefined_property k);
  x

module Obj = struct
  type t = js

  let t = global "Object"
  let empty () = Helix_js_external.obj_new t [||]

  let of_list entries =
    let obj = Helix_js_external.obj [||] in
    List.iter (fun (name, js) -> Helix_js_external.obj_set obj name js) entries;
    obj

  let of_array = Helix_js_external.obj

  (* Get *)

  let get obj k decode =
    let x = Helix_js_external.obj_get obj k in
    if is_undefined x then raise (Undefined_property k) else decode x

  let get_opt obj k decode =
    let x = Helix_js_external.obj_get obj k in
    if is_undefined x then None else Some (decode x)

  let rec get_path obj path decode =
    match path with
    | [] -> decode obj
    | k :: path' ->
      let obj' = Helix_js_external.obj_get obj k in
      if is_undefined obj' then raise (Undefined_property k)
      else get_path obj' path' decode

  let rec get_path_opt obj path decode =
    match path with
    | [] -> Some (decode obj)
    | k :: path' ->
      let obj' = Helix_js_external.obj_get obj k in
      if is_undefined obj' then None else get_path_opt obj' path' decode

  let get_js = Helix_js_external.obj_get

  (* Set *)

  let set obj k encode x =
    let x_js = encode x in
    Helix_js_external.obj_set obj k x_js

  let rec set_path obj path encode x =
    match path with
    | [] -> invalid_arg "Obj.set_path: empty path"
    | [ k ] -> Helix_js_external.obj_set obj k (encode x)
    | k :: path' ->
      let obj' = Helix_js_external.obj_get obj k in
      if is_undefined obj' then raise (Undefined_property k);
      set_path obj' path' encode x

  let set_js = Helix_js_external.obj_set

  (* Delete *)

  let del = Helix_js_external.obj_del
  let del_js = Helix_js_external.obj_del

  (* Function *)

  let call_js = Helix_js_external.obj_call

  let call_js_unit this f args =
    let _ = Helix_js_external.obj_call this f args in
    ()

  let call0 obj f_k ~return () =
    return (Helix_js_external.obj_call obj f_k [||])

  let call1 obj f_k ~return encode a =
    return (Helix_js_external.obj_call obj f_k [| encode a |])

  let call2 obj f_k ~return encode_a encode_b a b =
    return (Helix_js_external.obj_call obj f_k [| encode_a a; encode_b b |])

  let call3 obj f_k ~return encode_a encode_b encode_c a b c =
    return
      (Helix_js_external.obj_call obj f_k
         [| encode_a a; encode_b b; encode_c c |])

  let call4 obj f_k ~return encode_a encode_b encode_c encode_d a b c d =
    return
      (Helix_js_external.obj_call obj f_k
         [| encode_a a; encode_b b; encode_c c; encode_d d |])

  let call5 obj f_k ~return encode_a encode_b encode_c encode_d encode_e a b c d
      e =
    return
      (Helix_js_external.obj_call obj f_k
         [| encode_a a; encode_b b; encode_c c; encode_d d; encode_e e |])

  let call6 obj f_k ~return encode_a encode_b encode_c encode_d encode_e
      encode_f a b c d e f =
    return
      (Helix_js_external.obj_call obj f_k
         [|
           encode_a a;
           encode_b b;
           encode_c c;
           encode_d d;
           encode_e e;
           encode_f f;
         |])

  let call7 obj f_k ~return encode_a encode_b encode_c encode_d encode_e
      encode_f encode_g a b c d e f g =
    return
      (Helix_js_external.obj_call obj f_k
         [|
           encode_a a;
           encode_b b;
           encode_c c;
           encode_d d;
           encode_e e;
           encode_f f;
           encode_g g;
         |])

  let call8 obj f_k ~return encode_a encode_b encode_c encode_d encode_e
      encode_f encode_g encode_h a b c d e f g h =
    return
      (Helix_js_external.obj_call obj f_k
         [|
           encode_a a;
           encode_b b;
           encode_c c;
           encode_d d;
           encode_e e;
           encode_f f;
           encode_g g;
           encode_h h;
         |])

  let call9 obj f_k ~return encode_a encode_b encode_c encode_d encode_e
      encode_f encode_g encode_h encode_i a b c d e f g h i =
    return
      (Helix_js_external.obj_call obj f_k
         [|
           encode_a a;
           encode_b b;
           encode_c c;
           encode_d d;
           encode_e e;
           encode_f f;
           encode_g g;
           encode_h h;
           encode_i i;
         |])

  let call0_unit obj f_k () = ignore (Helix_js_external.obj_call obj f_k [||])

  let call1_unit obj f_k encode a =
    ignore (Helix_js_external.obj_call obj f_k [| encode a |])

  let call2_unit obj f_k encode_a encode_b a b =
    ignore (Helix_js_external.obj_call obj f_k [| encode_a a; encode_b b |])

  let call3_unit obj f_k encode_a encode_b encode_c a b c =
    ignore
      (Helix_js_external.obj_call obj f_k
         [| encode_a a; encode_b b; encode_c c |])

  let call4_unit obj f_k encode_a encode_b encode_c encode_d a b c d =
    ignore
      (Helix_js_external.obj_call obj f_k
         [| encode_a a; encode_b b; encode_c c; encode_d d |])

  let call5_unit obj f_k encode_a encode_b encode_c encode_d encode_e a b c d e
      =
    ignore
      (Helix_js_external.obj_call obj f_k
         [| encode_a a; encode_b b; encode_c c; encode_d d; encode_e e |])

  let call6_unit obj f_k encode_a encode_b encode_c encode_d encode_e encode_f a
      b c d e f =
    ignore
      (Helix_js_external.obj_call obj f_k
         [|
           encode_a a;
           encode_b b;
           encode_c c;
           encode_d d;
           encode_e e;
           encode_f f;
         |])

  let call7_unit obj f_k encode_a encode_b encode_c encode_d encode_e encode_f
      encode_g a b c d e f g =
    ignore
      (Helix_js_external.obj_call obj f_k
         [|
           encode_a a;
           encode_b b;
           encode_c c;
           encode_d d;
           encode_e e;
           encode_f f;
           encode_g g;
         |])

  let call8_unit obj f_k encode_a encode_b encode_c encode_d encode_e encode_f
      encode_g encode_h a b c d e f g h =
    ignore
      (Helix_js_external.obj_call obj f_k
         [|
           encode_a a;
           encode_b b;
           encode_c c;
           encode_d d;
           encode_e e;
           encode_f f;
           encode_g g;
           encode_h h;
         |])

  let call9_unit obj f_k encode_a encode_b encode_c encode_d encode_e encode_f
      encode_g encode_h encode_i a b c d e f g h i =
    ignore
      (Helix_js_external.obj_call obj f_k
         [|
           encode_a a;
           encode_b b;
           encode_c c;
           encode_d d;
           encode_e e;
           encode_f f;
           encode_g g;
           encode_h h;
           encode_i i;
         |])

  (* New *)
  let new0 obj = Helix_js_external.obj_new obj [||]
  let new1 obj encode a = Helix_js_external.obj_new obj [| encode a |]

  let new2 obj encode_a encode_b a b =
    Helix_js_external.obj_new obj [| encode_a a; encode_b b |]

  let new3 obj encode_a encode_b encode_c a b c =
    Helix_js_external.obj_new obj [| encode_a a; encode_b b; encode_c c |]

  let new4 obj encode_a encode_b encode_c encode_d a b c d =
    Helix_js_external.obj_new obj
      [| encode_a a; encode_b b; encode_c c; encode_d d |]

  let new5 obj encode_a encode_b encode_c encode_d encode_e a b c d e =
    Helix_js_external.obj_new obj
      [| encode_a a; encode_b b; encode_c c; encode_d d; encode_e e |]

  let new6 obj encode_a encode_b encode_c encode_d encode_e encode_f a b c d e f
      =
    Helix_js_external.obj_new obj
      [|
        encode_a a; encode_b b; encode_c c; encode_d d; encode_e e; encode_f f;
      |]

  let new7 obj encode_a encode_b encode_c encode_d encode_e encode_f encode_g a
      b c d e f g =
    Helix_js_external.obj_new obj
      [|
        encode_a a;
        encode_b b;
        encode_c c;
        encode_d d;
        encode_e e;
        encode_f f;
        encode_g g;
      |]

  let new8 obj encode_a encode_b encode_c encode_d encode_e encode_f encode_g
      encode_h a b c d e f g h =
    Helix_js_external.obj_new obj
      [|
        encode_a a;
        encode_b b;
        encode_c c;
        encode_d d;
        encode_e e;
        encode_f f;
        encode_g g;
        encode_h h;
      |]

  let new9 obj encode_a encode_b encode_c encode_d encode_e encode_f encode_g
      encode_h encode_i a b c d e f g h i =
    Helix_js_external.obj_new obj
      [|
        encode_a a;
        encode_b b;
        encode_c c;
        encode_d d;
        encode_e e;
        encode_f f;
        encode_g g;
        encode_h h;
        encode_i i;
      |]

  let new_js = Helix_js_external.obj_new
end

(* Encoder/decoder *)

type 'a encoder = 'a -> t

let encode f x = f x

module Encoder = struct
  let unit () = undefined
  let int = Helix_js_external.of_int
  let float = Helix_js_external.of_float
  let js x = x
  let bool = Helix_js_external.of_bool
  let string = Helix_js_external.of_string
  let array_js = Helix_js_external.of_js_array
  let array to_js array = Helix_js_external.of_js_array (Array.map to_js array)

  let pair to_js_x to_js_y (x, y) =
    let array = [| to_js_x x; to_js_y y |] in
    Helix_js_external.of_js_array array

  let triple to_js_x to_js_y to_js_z (x, y, z) =
    let array = [| to_js_x x; to_js_y y; to_js_z z |] in
    Helix_js_external.of_js_array array

  let nullable to_js nullable =
    match nullable with
    | None -> null
    | Some x -> to_js x

  let optional to_js optional =
    match optional with
    | None -> undefined
    | Some x -> to_js x

  let obj = Obj.of_list
  let fun0 f = Helix_js_external.of_fun 1 f
  let fun1 f = Helix_js_external.of_fun 1 f
  let fun2 f = Helix_js_external.of_fun 2 f
  let fun3 f = Helix_js_external.of_fun 3 f
  let fun4 f = Helix_js_external.of_fun 4 f
  let fun5 f = Helix_js_external.of_fun 5 f
  let fun6 f = Helix_js_external.of_fun 6 f
  let fun7 f = Helix_js_external.of_fun 7 f
  let fun8 f = Helix_js_external.of_fun 8 f
  let fun9 f = Helix_js_external.of_fun 9 f
  let any = of_any
end

type 'a decoder = t -> 'a

let decode f js = f js

module Decoder = struct
  let unit _ = ()
  let int = Helix_js_external.to_int
  let float = Helix_js_external.to_float
  let js x = x
  let bool = Helix_js_external.to_bool
  let string = Helix_js_external.to_string
  let array_js = Helix_js_external.to_js_array

  let array of_js array_js =
    Array.map of_js (Helix_js_external.to_js_array array_js)

  let pair of_js_x of_js_y js =
    let array = Helix_js_external.to_js_array js in
    match array with
    | [| x_js; y_js |] -> (of_js_x x_js, of_js_y y_js)
    | _ -> invalid_arg "array does not represent a pair"

  let triple of_js_x of_js_y of_js_z js =
    let array = Helix_js_external.to_js_array js in
    match array with
    | [| x_js; y_js; z_js |] -> (of_js_x x_js, of_js_y y_js, of_js_z z_js)
    | _ -> invalid_arg "array does not represent a triple"

  let nullable of_js js = if is_null js then None else Some (of_js js)
  let optional of_js js = if is_undefined js then None else Some (of_js js)
  let field js name decoder = decoder (Helix_js_external.obj_get js name)
  let any = to_any
end

(* Functions *)

module Fun = struct
  type t = js

  let call0 f_js ~return () = return (Helix_js_external.fun_call f_js [||])

  let call1 f_js ~return encode a =
    return (Helix_js_external.fun_call f_js [| encode a |])

  let call2 f_js ~return encode_a encode_b a b =
    return (Helix_js_external.fun_call f_js [| encode_a a; encode_b b |])

  let call3 f_js ~return encode_a encode_b encode_c a b c =
    return
      (Helix_js_external.fun_call f_js [| encode_a a; encode_b b; encode_c c |])

  let call4 f_js ~return encode_a encode_b encode_c encode_d a b c d =
    return
      (Helix_js_external.fun_call f_js
         [| encode_a a; encode_b b; encode_c c; encode_d d |])

  let call5 f_js ~return encode_a encode_b encode_c encode_d encode_e a b c d e
      =
    return
      (Helix_js_external.fun_call f_js
         [| encode_a a; encode_b b; encode_c c; encode_d d; encode_e e |])

  let call6 f_js ~return encode_a encode_b encode_c encode_d encode_e encode_f a
      b c d e f =
    return
      (Helix_js_external.fun_call f_js
         [|
           encode_a a;
           encode_b b;
           encode_c c;
           encode_d d;
           encode_e e;
           encode_f f;
         |])

  let call7 f_js ~return encode_a encode_b encode_c encode_d encode_e encode_f
      encode_g a b c d e f g =
    return
      (Helix_js_external.fun_call f_js
         [|
           encode_a a;
           encode_b b;
           encode_c c;
           encode_d d;
           encode_e e;
           encode_f f;
           encode_g g;
         |])

  let call8 f_js ~return encode_a encode_b encode_c encode_d encode_e encode_f
      encode_g encode_h a b c d e f g h =
    return
      (Helix_js_external.fun_call f_js
         [|
           encode_a a;
           encode_b b;
           encode_c c;
           encode_d d;
           encode_e e;
           encode_f f;
           encode_g g;
           encode_h h;
         |])

  let call9 f_js ~return encode_a encode_b encode_c encode_d encode_e encode_f
      encode_g encode_h encode_i a b c d e f g h i =
    return
      (Helix_js_external.fun_call f_js
         [|
           encode_a a;
           encode_b b;
           encode_c c;
           encode_d d;
           encode_e e;
           encode_f f;
           encode_g g;
           encode_h h;
           encode_i i;
         |])

  let call_js = Helix_js_external.fun_call

  let call_js_unit f args =
    let _ = Helix_js_external.fun_call f args in
    ()
end

module Dict = struct
  type 'a t = js

  let empty = Obj.empty

  let of_array arr =
    let arr =
      (Stdlib_obj.magic : (string * 'a) array -> (string * js) array) arr
    in
    Helix_js_external.obj arr

  let of_list l = of_array (Stdlib_array.of_list l)
  let get dict key = Obj.get dict key Decoder.any
  let get_opt dict key = Obj.get_opt dict key Decoder.any
  let set dict key x = Obj.set dict key Encoder.any x
  let del = Obj.del

  let entry_of_js entry_js =
    match Decoder.array_js entry_js with
    | [| key; v |] -> (Decoder.string key, Decoder.any v)
    | _ -> invalid_arg "Object entries is not a pair"

  let entries dict =
    Obj.call1 Obj.t "entries"
      ~return:(Decoder.array entry_of_js)
      Encoder.js dict

  let keys dict =
    Obj.call1 Obj.t "keys" ~return:Decoder.(array string) Encoder.js dict

  let values dict =
    Obj.call1 Obj.t "values" ~return:Decoder.(array any) Encoder.js dict

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
end

module Array = struct
  type 'a t = js

  let t = global "Array"
  let make n = Obj.new1 t Encoder.int n
  let set arr i x = Helix_js_external.obj_set arr i (Encoder.any x)

  let init n f =
    let out = make n in
    for i = 0 to n - 1 do
      set out i (f i)
    done;
    out

  let get arr i = Decoder.any (Helix_js_external.obj_get arr i)

  let get_opt arr i =
    let x = get arr i in
    let x_js = Encoder.any x in
    if is_undefined x_js then raise Not_found else x

  let push arr x = Obj.call1_unit arr "push" Encoder.any x
  let pop arr = Obj.call0 arr "pop" ~return:Decoder.any ()
  let pop_opt arr = Obj.call0 arr "pop" ~return:Decoder.(optional any) ()
  let length arr = Obj.get arr "length" Decoder.int
  let iter arr f = Obj.call1_unit arr "forEach" Encoder.fun1 f
end
