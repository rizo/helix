open struct
  module Promise = Stdweb_promise
  module Form_data = Stdweb_form_data
  module E = Jx.Encode
  module D = Jx.Decode
end

(* Request *)

type request = Jx.t

module Request = struct
  type t = request

  let t = Jx.global "Request"

  let make ?options url =
    match options with
    | Some options -> Jx.obj_new t [| E.string url; options |]
    | None -> Jx.obj_new t [| E.string url |]

  let of_js x = x
  let to_js x = x
end

module Body = struct
  type t = Jx.t

  let of_string = E.string
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
  let make () = Jx.obj_new t [||]

  let append t name value =
    ignore (Jx.obj_call t "append" [| E.string name; E.string value |])

  let entries t = Jx.obj_call t "entries" [||]
end

module Options = struct
  type t = options

  let make ?body ?meth ?(headers = []) ?mode ?credentials () =
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
      | Some `Cors -> ("mode", E.string "cors") :: fields
      | Some `No_cors -> ("mode", E.string "no-cors") :: fields
      | None -> fields
    in
    let fields =
      match credentials with
      | Some `Omit -> ("credentials", E.string "omit") :: fields
      | Some `Same_origin -> ("credentials", E.string "same-origin") :: fields
      | Some `Include -> ("credentials", E.string "include") :: fields
      | None -> fields
    in
    let fields =
      match body with
      | Some body -> ("body", Body.to_js body) :: fields
      | None -> fields
    in
    let fields =
      match meth with
      | Some `Get -> ("method", E.string "GET") :: fields
      | Some `Put -> ("method", E.string "PUT") :: fields
      | Some `Post -> ("method", E.string "POST") :: fields
      | Some `Delete -> ("method", E.string "DELETE") :: fields
      | Some `Head -> ("method", E.string "HEAD") :: fields
      | Some `Connect -> ("method", E.string "CONNECT") :: fields
      | Some `Options -> ("method", E.string "OPTIONS") :: fields
      | Some `Trace -> ("method", E.string "TRACE") :: fields
      | None -> fields
    in
    E.obj (Array.of_list fields)

  let of_js x = x
  let to_js x = x
end

(* Response *)

type response = Jx.t

module Response = struct
  type t = response

  let ok t = D.bool (Jx.obj_get t "ok")
  let status t = D.int (Jx.obj_get t "status")
  let text t = Jx.obj_call t "text" [||] |> Promise.map D.string
  let json t = Jx.obj_call t "json" [||] |> Promise.map (fun x -> x)
  let headers t = Jx.obj_get t "headers"
  let of_js x = x
  let to_js x = x
end

let fetch ?body ?(meth = `Get) ?headers ?mode ?credentials url =
  let opts = Options.make ~meth ?headers ?mode ?credentials ?body () in
  Jx.obj_call Stdweb_global.window "fetch"
    [| E.string url; Options.to_js opts |]
