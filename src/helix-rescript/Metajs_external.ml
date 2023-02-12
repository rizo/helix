type js

external global : js = "globalThis" [@@bs.val]

let null : js = [%raw "null"]
let undefined : js = [%raw "undefined"]
let js_equal = ( = )

external js_of_string : string -> js = "%identity"
external string_of_js : js -> string = "%identity"
external js_of_bytestring : string -> js = "%identity"
external bytestring_of_js : js -> string = "%identity"
external js_of_bool : bool -> js = "%identity"
external bool_of_js : js -> bool = "%identity"
external js_of_float : float -> js = "%identity"
external float_of_js : js -> float = "%identity"
external js_of_int : int -> js = "%identity"
external int_of_js : js -> int = "%identity"
external js_of_array : 'a array -> js = "%identity"
external array_of_js : js -> 'a array = "%identity"

let obj : (string * js) array -> js =
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

let new_obj : js -> js array -> js =
  [%raw {|function(c, a) { return new c(...a); }|}]

let get : js -> string -> js =
  [%raw {|function(obj, prop) { return obj[prop]; }|}]

let set : js -> string -> js -> unit =
  [%raw {|function(obj, prop, value) { obj[prop] = value; }|}]

let delete : js -> string -> unit =
  [%raw {|function(obj, prop) { delete o[prop]; }|}]

let fun_call : js -> js array -> js =
  [%raw {|function(f, a) { return f(...a); }|}]

let meth_call : js -> string -> js array -> js =
  [%raw {|function(o, n, a) { return o[n].apply(o, a); }|}]

let callback ~arity:_ (f : _ -> _) : js = Obj.magic f
let typeof : js -> js = [%raw {|function(o) { return typeof o; }|}]

let instanceof : js -> js -> bool =
  [%raw {|function(obj, cls) { return obj instanceof cls; }|}]
