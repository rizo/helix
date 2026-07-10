open struct
  module Dom = Stdweb_dom
  module Object = Stdweb_object
  module E = Jx.Encode
  module D = Jx.Decode
end

type t = Jx.t

let constr = Jx.global "FormData"

let make ?submitter form =
  match submitter with
  | None -> Jx.obj_new constr [| Dom.Node.to_js form |]
  | Some submitter ->
    Jx.obj_new constr [| Dom.Node.to_js form; Dom.Node.to_js submitter |]

let entries t =
  let iter_js = Jx.obj_call t "entries" [||] in
  Stdweb_iterator.unsafe_of_js iter_js

let to_json = Object.from_entries
let to_js t = t

let to_assoc t =
  let it = entries t in
  let out = ref [] in
  Stdweb_iterator.iter
    (fun x ->
      let arr : Jx.t array = D.array x in
      let item = (D.string (Array.get arr 0), D.string (Array.get arr 1)) in
      out := item :: !out
    )
    it;
  List.rev !out
