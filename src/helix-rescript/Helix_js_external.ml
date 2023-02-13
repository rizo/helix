type t

external global : t = "globalThis" [@@bs.val]

let null : t = [%raw "null"]
let undefined : t = [%raw "undefined"]
let equal = ( = )

external of_string : string -> t = "%identity"
external to_string : t -> string = "%identity"
external of_bool : bool -> t = "%identity"
external to_bool : t -> bool = "%identity"
external of_float : float -> t = "%identity"
external to_float : t -> float = "%identity"
external of_int : int -> t = "%identity"
external to_int : t -> int = "%identity"
external of_js_array : 'a array -> t = "%identity"
external to_js_array : t -> 'a array = "%identity"

let obj : (string * t) array -> t =
  [%raw
    {|
function(a) {
  var o = {};
  for (var i = 1; i < a.length; i++) {
    var p = a[i];
    o[p[1]] = p[2];
  }
  return o;
}
|}]

let obj_new : t -> t array -> t =
  [%raw {|function(c, a) { return new c(...a); }|}]

let obj_get : t -> string -> t =
  [%raw {|function(obj, prop) { return obj[prop]; }|}]

let obj_set : t -> string -> t -> unit =
  [%raw {|function(obj, prop, value) { obj[prop] = value; }|}]

let obj_del : t -> string -> unit =
  [%raw {|function(obj, prop) { delete o[prop]; }|}]

let fun_call : t -> t array -> t = [%raw {|function(f, a) { return f(...a); }|}]

let obj_call : t -> string -> t array -> t =
  [%raw {|function(o, n, a) { return o[n].apply(o, a); }|}]

let of_fun _ (f : _ -> _) : t = Obj.magic f
let typeof : t -> t = [%raw {|function(o) { return typeof o; }|}]

let instanceof : t -> t -> bool =
  [%raw {|function(obj, cls) { return obj instanceof cls; }|}]
