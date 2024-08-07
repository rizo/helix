module Fetch = Stdweb.Fetch
module Promise = Stdweb.Promise

type error =
  | Fetch_error of Jx.t
  | Unsuccessful of Stdweb.Fetch.Response.t
  | Decoding_error of exn
  | Handling_error of exn

let string_of_error = function
  | Fetch_error _ -> "Fetch error"
  | Unsuccessful resp ->
    "Unsuccessful response: " ^ string_of_int (Stdweb.Fetch.Response.status resp)
  | Decoding_error _ -> "Decoding failed"
  | Handling_error _ -> "Could not handle fetched payload"

type nonrec 'a result = ('a, error) result
type body = Stdweb.Fetch.Body.t

module Encoder = struct
  type 'a t = string option * ('a -> Fetch.Body.t)

  let make ?(content_type) f = content_type, f

  let to_undefined_body _ = Fetch.Body.unsafe_of_js Jx.undefined
  let empty = (None, to_undefined_body)
  let ignore = (None, to_undefined_body)
  let text = (None, Fetch.Body.of_string)
  let form_data = (None, Fetch.Body.of_form_data)

  let assoc_to_form_urlencoded_body assoc =
    let string =
      String.concat "&"
        (List.map
           (fun (k, v) ->
             String.concat "="
               [ Stdweb.encode_uri_component k; Stdweb.encode_uri_component v ]
           )
           assoc
        )
    in
    Fetch.Body.of_string string

  let form_urlencoded =
    ( Some "application/x-www-form-urlencoded; charset=UTF-8",
      assoc_to_form_urlencoded_body
    )

  let json_to_body json =
    let text = Stdweb.Json.stringify json in
    Fetch.Body.of_string text

  let json = (Some "application/json", json_to_body)

  let map_json encode_json =
    (Some "application/json", fun x -> json_to_body (encode_json x))
end

module Decoder = struct
  type 'a t = Fetch.Response.t -> 'a Promise.t

  let ignore _res = Promise.resolve ()
  let text = Fetch.Response.text
  let json = Fetch.Response.json

  let map f t response =
    t response
    |> Promise.and_then (fun x ->
           match f x with
           | Ok x -> Promise.resolve x
           | Error err -> Promise.reject err
       )

  let map_json f = map f json
end

(* FIXME: prevent signal handler errors from being caught *)
let request ?meth ?(headers = []) ?mode ?credentials
    ~encode:(content_type, to_body) ~decode ~url request_value =
  let headers =
    match content_type with
    | Some content_type -> ("Content-Type", content_type) :: headers
    | None -> headers
  in
  let body = to_body request_value in
  let s = Signal.make None in
  Fetch.fetch ~body ?meth ~headers ?mode ?credentials url
  |> Promise.and_then (fun response ->
         let () =
           if Fetch.Response.ok response then
             decode response
             |> Promise.and_then (fun text ->
                    let () =
                      try Signal.emit (Some (Ok text)) s
                      with exn ->
                        Signal.emit (Some (Error (Handling_error exn))) s
                    in
                    Promise.resolve ()
                )
             |> Promise.catch (fun err ->
                    Signal.emit (Some (Error (Decoding_error err))) s
                )
           else
             let err = Error (Unsuccessful response) in
             Signal.emit (Some err) s
         in
         Promise.resolve ()
     )
  |> Promise.catch (fun err -> Signal.emit (Some (Error (Fetch_error err))) s);
  s

let get ?headers ?mode ?credentials ~decode ~url () =
  request ~meth:`Get ?headers ?mode ?credentials ~encode:Encoder.ignore ~decode
    ~url ()

let put ?headers ?mode ?credentials ~encode ~decode ~url request_value =
  request ~meth:`Put ?headers ?mode ?credentials ~encode ~decode ~url
    request_value

let post ?headers ?mode ?credentials ~encode ~decode ~url request_value =
  request ~meth:`Post ?headers ?mode ?credentials ~encode ~decode ~url
    request_value

let delete ?headers ?mode ?credentials ~decode ~url request_value =
  request ~meth:`Delete ?headers ?mode ?credentials ~encode:Encoder.ignore
    ~decode ~url request_value

module Json = struct
  let request ?meth ?headers ?mode ?credentials ~encode ~decode =
    request ?meth ?headers ?mode ?credentials ~encode:(Encoder.map_json encode)
      ~decode:(Decoder.map_json decode)

  let get ?headers ?mode ?credentials ~decode =
    get ?headers ?mode ?credentials ~decode:(Decoder.map_json decode)

  let put ?headers ?mode ?credentials ~encode ~decode =
    put ?headers ?mode ?credentials ~encode:(Encoder.map_json encode)
      ~decode:(Decoder.map_json decode)

  let post ?headers ?mode ?credentials ~encode ~decode =
    post ?headers ?mode ?credentials ~encode:(Encoder.map_json encode)
      ~decode:(Decoder.map_json decode)

  let delete ?headers ?mode ?credentials ~decode =
    delete ?headers ?mode ?credentials ~decode:(Decoder.map_json decode)
end
