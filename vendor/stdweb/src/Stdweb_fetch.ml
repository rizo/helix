open struct
  module Promise = Stdweb_promise
  module Form_data = Stdweb_form_data
end

(* Request *)

type request = Jx.t

module Request = struct
  type t = request

  let t = Jx.global "Request"

  let make ?options url =
    match options with
    | Some options -> Jx.Obj.new2 t Jx.Encoder.string Jx.Encoder.js url options
    | None -> Jx.Obj.new1 t Jx.Encoder.string url

  let of_js = Jx.Decoder.js
  let to_js = Jx.Encoder.js
end

module Body = struct
  type t = Jx.t

  let of_string = Jx.Encoder.string
  let of_form_data = Form_data.to_js
  let to_js t = t
  let unsafe_of_js t = t
end

(* Options *)

type meth =
  [ `Get | `Put | `Post | `Delete | `Head | `Connect | `Trace | `Options ]

type mode = [ `Cors | `No_cors ]
type credentials = [ `Omit | `Same_origin | `Include ]
type options = Jx.t
type headers = Jx.t

module Headers = struct
  type t = headers

  let t = Jx.global "Headers"
  let make () = Jx.Obj.new0 t

  let append t name value =
    Jx.Obj.call2_unit t "append" Jx.Encoder.string Jx.Encoder.string name value

  let entries t = Jx.Obj.call0 t "entries" ~return:Jx.Decoder.js ()
end

module Options = struct
  type t = options

  let make ?body ?meth ?(headers = []) ?mode ?credentials () =
    let open Jx.Encoder in
    let fields = [] in
    let fields =
      match headers with
      | [] -> fields
      | _ ->
        let hs = Headers.make () in
        List.iter (fun (name, value) -> Headers.append hs name value) headers;
        ("headers", hs) :: fields
    in
    let fields =
      match mode with
      | Some `Cors -> ("mode", string "cors") :: fields
      | Some `No_cors -> ("mode", string "no-cors") :: fields
      | None -> fields
    in
    let fields =
      match credentials with
      | Some `Omit -> ("credentials", string "omit") :: fields
      | Some `Same_origin -> ("credentials", string "same-origin") :: fields
      | Some `Include -> ("credentials", string "include") :: fields
      | None -> fields
    in
    let fields =
      match body with
      | Some body -> ("body", Body.to_js body) :: fields
      | None -> fields
    in
    let fields =
      match meth with
      | Some `Get -> ("method", string "GET") :: fields
      | Some `Put -> ("method", string "PUT") :: fields
      | Some `Post -> ("method", string "POST") :: fields
      | Some `Delete -> ("method", string "DELETE") :: fields
      | Some `Head -> ("method", string "HEAD") :: fields
      | Some `Connect -> ("method", string "CONNECT") :: fields
      | Some `Options -> ("method", string "OPTIONS") :: fields
      | Some `Trace -> ("method", string "TRACE") :: fields
      | None -> fields
    in
    Jx.Obj.of_list fields

  let of_js = Jx.Decoder.js
  let to_js = Jx.Encoder.js
end

(* Response *)

type response = Jx.js

module Response = struct
  type t = response

  let ok t = Jx.Obj.get t "ok" Jx.Decoder.bool
  let status t = Jx.Obj.get t "status" Jx.Decoder.int

  let text t =
    Jx.Obj.call0 t "text" ~return:Promise.of_js ()
    |> Promise.map Jx.Decoder.string

  let json t =
    Jx.Obj.call0 t "json" ~return:Promise.of_js () |> Promise.map Jx.Decoder.js

  let headers t = Jx.Obj.get t "headers" Jx.Decoder.js
  let of_js = Jx.Decoder.js
  let to_js = Jx.Encoder.js
end

let fetch ?body ?(meth = `Get) ?headers ?mode ?credentials url =
  let opts = Options.make ~meth ?headers ?mode ?credentials ?body () in
  Jx.Obj.call2 Stdweb_global.window "fetch" ~return:Response.of_js
    Jx.Encoder.string Options.to_js url opts
