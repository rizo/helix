(* External *)

type t = Jx_virt.t
type js = t

let global_this = Jx_virt.global_this
let null = Jx_virt.null
let undefined = Jx_virt.undefined
let equal = Jx_virt.equal
let debugger = Jx_virt.debugger
let type_of x = Jx_virt.to_string (Jx_virt.type_of x)
let instance_of x ~constr = Jx_virt.instance_of x constr
let is_null v = v == null
let is_undefined v = v == undefined
let is_defined v = v != undefined

exception Undefined_property of string

external of_any : 'a -> Jx_virt.t = "%identity"
external to_any : Jx_virt.t -> 'a = "%identity"

let global k =
  let x = Jx_virt.obj_get global_this (of_any k) in
  if is_undefined x then raise (Undefined_property k);
  x

module Obj = struct
  type t = js

  let of_js js = js
  let to_js obj = obj
  let t = global "Object"
  let empty () = Jx_virt.obj_new t [||]

  let of_list entries =
    let obj = Jx_virt.obj [||] in
    List.iter (fun (name, js) -> Jx_virt.obj_set obj name js) entries;
    obj

  let of_array = Jx_virt.obj
  let create obj = Jx_virt.obj_call t "create" [| obj |]
  let get_prototype obj = Jx_virt.obj_get obj "prototype"
  let set_prototype obj proto = Jx_virt.obj_set obj "prototype" proto

  (* Get *)

  let get obj k decode =
    let x = Jx_virt.obj_get obj k in
    if is_undefined x then raise (Undefined_property k) else decode x

  let get_opt obj k decode =
    let x = Jx_virt.obj_get obj k in
    if is_undefined x then None else Some (decode x)

  let rec get_path obj path decode =
    match path with
    | [] -> decode obj
    | k :: path' ->
      let obj' = Jx_virt.obj_get obj k in
      if is_undefined obj' then raise (Undefined_property k)
      else get_path obj' path' decode

  let rec get_path_opt obj path decode =
    match path with
    | [] -> Some (decode obj)
    | k :: path' ->
      let obj' = Jx_virt.obj_get obj k in
      if is_undefined obj' then None else get_path_opt obj' path' decode

  let get_js = Jx_virt.obj_get

  (* Set *)

  let set obj k encode x =
    let x_js = encode x in
    Jx_virt.obj_set obj k x_js

  let rec set_path obj path encode x =
    match path with
    | [] -> invalid_arg "Obj.set_path: empty path"
    | [ k ] -> Jx_virt.obj_set obj k (encode x)
    | k :: path' ->
      let obj' = Jx_virt.obj_get obj k in
      if is_undefined obj' then raise (Undefined_property k);
      set_path obj' path' encode x

  let set_js = Jx_virt.obj_set

  (* Delete *)

  let del = Jx_virt.obj_del
  let del_js = Jx_virt.obj_del

  (* Has *)

  let has obj k =
    let x = Jx_virt.obj_get obj k in
    is_defined x

  let has_js obj k_js =
    let x = Jx_virt.obj_get obj k_js in
    is_defined x

  (* Function *)

  let call_js = Jx_virt.obj_call

  let call_js_unit this f args =
    let _ = Jx_virt.obj_call this f args in
    ()

  let call0 obj f_k ~return () = return (Jx_virt.obj_call obj f_k [||])

  let call1 obj f_k ~return encode a =
    return (Jx_virt.obj_call obj f_k [| encode a |])

  let call2 obj f_k ~return encode_a encode_b a b =
    return (Jx_virt.obj_call obj f_k [| encode_a a; encode_b b |])

  let call3 obj f_k ~return encode_a encode_b encode_c a b c =
    return (Jx_virt.obj_call obj f_k [| encode_a a; encode_b b; encode_c c |])

  let call4 obj f_k ~return encode_a encode_b encode_c encode_d a b c d =
    return
      (Jx_virt.obj_call obj f_k
         [| encode_a a; encode_b b; encode_c c; encode_d d |]
      )

  let call5 obj f_k ~return encode_a encode_b encode_c encode_d encode_e a b c d
      e =
    return
      (Jx_virt.obj_call obj f_k
         [| encode_a a; encode_b b; encode_c c; encode_d d; encode_e e |]
      )

  let call6 obj f_k ~return encode_a encode_b encode_c encode_d encode_e
      encode_f a b c d e f =
    return
      (Jx_virt.obj_call obj f_k
         [|
           encode_a a;
           encode_b b;
           encode_c c;
           encode_d d;
           encode_e e;
           encode_f f;
         |]
      )

  let call7 obj f_k ~return encode_a encode_b encode_c encode_d encode_e
      encode_f encode_g a b c d e f g =
    return
      (Jx_virt.obj_call obj f_k
         [|
           encode_a a;
           encode_b b;
           encode_c c;
           encode_d d;
           encode_e e;
           encode_f f;
           encode_g g;
         |]
      )

  let call8 obj f_k ~return encode_a encode_b encode_c encode_d encode_e
      encode_f encode_g encode_h a b c d e f g h =
    return
      (Jx_virt.obj_call obj f_k
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
      )

  let call9 obj f_k ~return encode_a encode_b encode_c encode_d encode_e
      encode_f encode_g encode_h encode_i a b c d e f g h i =
    return
      (Jx_virt.obj_call obj f_k
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
      )

  let call0_unit obj f_k () = ignore (Jx_virt.obj_call obj f_k [||])

  let call1_unit obj f_k encode a =
    ignore (Jx_virt.obj_call obj f_k [| encode a |])

  let call2_unit obj f_k encode_a encode_b a b =
    ignore (Jx_virt.obj_call obj f_k [| encode_a a; encode_b b |])

  let call3_unit obj f_k encode_a encode_b encode_c a b c =
    ignore (Jx_virt.obj_call obj f_k [| encode_a a; encode_b b; encode_c c |])

  let call4_unit obj f_k encode_a encode_b encode_c encode_d a b c d =
    ignore
      (Jx_virt.obj_call obj f_k
         [| encode_a a; encode_b b; encode_c c; encode_d d |]
      )

  let call5_unit obj f_k encode_a encode_b encode_c encode_d encode_e a b c d e
      =
    ignore
      (Jx_virt.obj_call obj f_k
         [| encode_a a; encode_b b; encode_c c; encode_d d; encode_e e |]
      )

  let call6_unit obj f_k encode_a encode_b encode_c encode_d encode_e encode_f a
      b c d e f =
    ignore
      (Jx_virt.obj_call obj f_k
         [|
           encode_a a;
           encode_b b;
           encode_c c;
           encode_d d;
           encode_e e;
           encode_f f;
         |]
      )

  let call7_unit obj f_k encode_a encode_b encode_c encode_d encode_e encode_f
      encode_g a b c d e f g =
    ignore
      (Jx_virt.obj_call obj f_k
         [|
           encode_a a;
           encode_b b;
           encode_c c;
           encode_d d;
           encode_e e;
           encode_f f;
           encode_g g;
         |]
      )

  let call8_unit obj f_k encode_a encode_b encode_c encode_d encode_e encode_f
      encode_g encode_h a b c d e f g h =
    ignore
      (Jx_virt.obj_call obj f_k
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
      )

  let call9_unit obj f_k encode_a encode_b encode_c encode_d encode_e encode_f
      encode_g encode_h encode_i a b c d e f g h i =
    ignore
      (Jx_virt.obj_call obj f_k
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
      )

  (* New *)
  let new0 obj = Jx_virt.obj_new obj [||]
  let new1 obj encode a = Jx_virt.obj_new obj [| encode a |]

  let new2 obj encode_a encode_b a b =
    Jx_virt.obj_new obj [| encode_a a; encode_b b |]

  let new3 obj encode_a encode_b encode_c a b c =
    Jx_virt.obj_new obj [| encode_a a; encode_b b; encode_c c |]

  let new4 obj encode_a encode_b encode_c encode_d a b c d =
    Jx_virt.obj_new obj [| encode_a a; encode_b b; encode_c c; encode_d d |]

  let new5 obj encode_a encode_b encode_c encode_d encode_e a b c d e =
    Jx_virt.obj_new obj
      [| encode_a a; encode_b b; encode_c c; encode_d d; encode_e e |]

  let new6 obj encode_a encode_b encode_c encode_d encode_e encode_f a b c d e f
      =
    Jx_virt.obj_new obj
      [|
        encode_a a; encode_b b; encode_c c; encode_d d; encode_e e; encode_f f;
      |]

  let new7 obj encode_a encode_b encode_c encode_d encode_e encode_f encode_g a
      b c d e f g =
    Jx_virt.obj_new obj
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
    Jx_virt.obj_new obj
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
    Jx_virt.obj_new obj
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

  let new_js = Jx_virt.obj_new
end

(* Encoder/decoder *)

type 'a encoder = 'a -> t

let encode f x = f x

module Encoder = struct
  let unit () = undefined
  let int = Jx_virt.of_int
  let float = Jx_virt.of_float
  let js x = x
  let bool = Jx_virt.of_bool
  let string = Jx_virt.of_string
  let array_js = Jx_virt.of_js_array
  let array to_js array = Jx_virt.of_js_array (Array.map to_js array)

  let pair to_js_x to_js_y (x, y) =
    let array = [| to_js_x x; to_js_y y |] in
    Jx_virt.of_js_array array

  let triple to_js_x to_js_y to_js_z (x, y, z) =
    let array = [| to_js_x x; to_js_y y; to_js_z z |] in
    Jx_virt.of_js_array array

  let nullable to_js nullable =
    match nullable with
    | None -> null
    | Some x -> to_js x

  let optional to_js optional =
    match optional with
    | None -> undefined
    | Some x -> to_js x

  let obj = Obj.of_list
  let fun0 f = Jx_virt.of_fun 1 f
  let fun1 f = Jx_virt.of_fun 1 f
  let fun2 f = Jx_virt.of_fun 2 f
  let fun3 f = Jx_virt.of_fun 3 f
  let fun4 f = Jx_virt.of_fun 4 f
  let fun5 f = Jx_virt.of_fun 5 f
  let fun6 f = Jx_virt.of_fun 6 f
  let fun7 f = Jx_virt.of_fun 7 f
  let fun8 f = Jx_virt.of_fun 8 f
  let fun9 f = Jx_virt.of_fun 9 f
  let any = of_any
end

type 'a decoder = t -> 'a

let decode f js = f js

module Decoder = struct
  let unit _ = ()
  let int = Jx_virt.to_int
  let float = Jx_virt.to_float
  let js x = x
  let bool = Jx_virt.to_bool
  let string = Jx_virt.to_string
  let array_js = Jx_virt.to_js_array
  let array of_js array_js = Array.map of_js (Jx_virt.to_js_array array_js)

  let pair of_js_x of_js_y js =
    let array = Jx_virt.to_js_array js in
    match array with
    | [| x_js; y_js |] -> (of_js_x x_js, of_js_y y_js)
    | _ -> invalid_arg "array does not represent a pair"

  let triple of_js_x of_js_y of_js_z js =
    let array = Jx_virt.to_js_array js in
    match array with
    | [| x_js; y_js; z_js |] -> (of_js_x x_js, of_js_y y_js, of_js_z z_js)
    | _ -> invalid_arg "array does not represent a triple"

  let nullable of_js js = if is_null js then None else Some (of_js js)
  let optional of_js js = if is_undefined js then None else Some (of_js js)
  let field js name decoder = decoder (Jx_virt.obj_get js name)
  let any = to_any
end

(* Functions *)

module Fun = struct
  type t = js

  let call0 f_js ~return () = return (Jx_virt.fun_call f_js [||])
  let call1 f_js ~return encode a = return (Jx_virt.fun_call f_js [| encode a |])

  let call2 f_js ~return encode_a encode_b a b =
    return (Jx_virt.fun_call f_js [| encode_a a; encode_b b |])

  let call3 f_js ~return encode_a encode_b encode_c a b c =
    return (Jx_virt.fun_call f_js [| encode_a a; encode_b b; encode_c c |])

  let call4 f_js ~return encode_a encode_b encode_c encode_d a b c d =
    return
      (Jx_virt.fun_call f_js
         [| encode_a a; encode_b b; encode_c c; encode_d d |]
      )

  let call5 f_js ~return encode_a encode_b encode_c encode_d encode_e a b c d e
      =
    return
      (Jx_virt.fun_call f_js
         [| encode_a a; encode_b b; encode_c c; encode_d d; encode_e e |]
      )

  let call6 f_js ~return encode_a encode_b encode_c encode_d encode_e encode_f a
      b c d e f =
    return
      (Jx_virt.fun_call f_js
         [|
           encode_a a;
           encode_b b;
           encode_c c;
           encode_d d;
           encode_e e;
           encode_f f;
         |]
      )

  let call7 f_js ~return encode_a encode_b encode_c encode_d encode_e encode_f
      encode_g a b c d e f g =
    return
      (Jx_virt.fun_call f_js
         [|
           encode_a a;
           encode_b b;
           encode_c c;
           encode_d d;
           encode_e e;
           encode_f f;
           encode_g g;
         |]
      )

  let call8 f_js ~return encode_a encode_b encode_c encode_d encode_e encode_f
      encode_g encode_h a b c d e f g h =
    return
      (Jx_virt.fun_call f_js
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
      )

  let call9 f_js ~return encode_a encode_b encode_c encode_d encode_e encode_f
      encode_g encode_h encode_i a b c d e f g h i =
    return
      (Jx_virt.fun_call f_js
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
      )

  let call0_unit f_js () = ignore (Jx_virt.fun_call f_js [||])
  let call1_unit f_js encode a = ignore (Jx_virt.fun_call f_js [| encode a |])

  let call2_unit f_js encode_a encode_b a b =
    ignore (Jx_virt.fun_call f_js [| encode_a a; encode_b b |])

  let call3_unit f_js encode_a encode_b encode_c a b c =
    ignore (Jx_virt.fun_call f_js [| encode_a a; encode_b b; encode_c c |])

  let call4_unit f_js encode_a encode_b encode_c encode_d a b c d =
    ignore
      (Jx_virt.fun_call f_js
         [| encode_a a; encode_b b; encode_c c; encode_d d |]
      )

  let call5_unit f_js encode_a encode_b encode_c encode_d encode_e a b c d e =
    ignore
      (Jx_virt.fun_call f_js
         [| encode_a a; encode_b b; encode_c c; encode_d d; encode_e e |]
      )

  let call6_unit f_js encode_a encode_b encode_c encode_d encode_e encode_f a b
      c d e f =
    ignore
      (Jx_virt.fun_call f_js
         [|
           encode_a a;
           encode_b b;
           encode_c c;
           encode_d d;
           encode_e e;
           encode_f f;
         |]
      )

  let call7_unit f_js encode_a encode_b encode_c encode_d encode_e encode_f
      encode_g a b c d e f g =
    ignore
      (Jx_virt.fun_call f_js
         [|
           encode_a a;
           encode_b b;
           encode_c c;
           encode_d d;
           encode_e e;
           encode_f f;
           encode_g g;
         |]
      )

  let call8_unit f_js encode_a encode_b encode_c encode_d encode_e encode_f
      encode_g encode_h a b c d e f g h =
    ignore
      (Jx_virt.fun_call f_js
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
      )

  let call9_unit f_js encode_a encode_b encode_c encode_d encode_e encode_f
      encode_g encode_h encode_i a b c d e f g h i =
    ignore
      (Jx_virt.fun_call f_js
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
      )

  let call_js = Jx_virt.fun_call

  let call_js_unit f args =
    let _ = Jx_virt.fun_call f args in
    ()
end

let console = global "console"
let log x = Obj.call_js_unit console "log" [| Encoder.any x |]
