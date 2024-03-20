module Array = Stdweb_array
module Console = Stdweb_console
module Dict = Stdweb_dict
module Dom = Stdweb_dom
module Iterator = Stdweb_iterator
module Object = Stdweb_object
module Form_data = Stdweb_form_data
module Map = Stdweb_map
module Promise = Stdweb_promise
module Fetch = Stdweb_fetch
module Storage = Stdweb_storage
module Clipboard = Stdweb_clipboard
module Json = Stdweb_json
module Websocket = Stdweb_websocket
module Url_search_params = Stdweb_url_search_params

module Navigator = struct
  type t = Jx.t

  let clipboard t = Jx.Obj.get t "clipboard" Jx.Decoder.js
end

module Number = struct
  type t = float

  let to_precision t n =
    let t_js = Jx.Encoder.float t in
    Jx.Obj.call1 t_js "toPrecision" ~return:Jx.Decoder.string Jx.Encoder.int n
end

include Stdweb_global
