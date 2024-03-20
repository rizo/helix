open struct
  module Dom = Stdweb_dom
  module Object = Stdweb_object
end

type t = Jx.t

let constr = Jx.global "FormData"

let make ?submitter form =
  match submitter with
  | None -> Jx.Obj.new1 constr Dom.Node.to_js form
  | Some submitter ->
    Jx.Obj.new2 constr Dom.Node.to_js Dom.Node.to_js form submitter

let entries t =
  let iter_js = Jx.Obj.call_js t "entries" [||] in
  Stdweb_iterator.unsafe_of_js iter_js

let to_json = Object.from_entries
let to_js t = t

let to_assoc t =
  let it = entries t in
  let out = ref [] in
  Stdweb_iterator.iter
    (fun x ->
      let arr = Jx.Decoder.array Jx.Decoder.string x in
      let item = (Array.get arr 0, Array.get arr 1) in
      out := item :: !out
    )
    it;
  List.rev !out
