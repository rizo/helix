module E = Jx.Encode
module D = Jx.Decode

let is_null js = Jx.strict_equal js Jx.null
let is_undefined js = Jx.strict_equal js Jx.undefined
let nullable decode js = if is_null js then None else Some (decode js)
let optional decode js = if is_undefined js then None else Some (decode js)

let encode_optional encode = function
  | None -> Jx.undefined
  | Some x -> encode x

let call_unit obj meth args = ignore (Jx.obj_call obj meth args)
