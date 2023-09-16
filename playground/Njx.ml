module type S = sig
  type 'a codec

  val int : int codec
  val string : string codec
  val list : 'a codec -> 'a list codec
  val pair : 'a codec -> 'b codec -> ('a * 'b) codec
end

module M = struct
  type t = string
  type 'a codec = { encode : 'a -> t; decode : t -> 'a }

  let int = { encode = string_of_int; decode = int_of_string }
end
