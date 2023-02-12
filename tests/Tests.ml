open Helix

let main () =
  let open Html in
  div []
    [
      (* Show.main (); *)
      (* Conditional_attr.main (); *)
      Each.main ();
    ]

let () =
  let open Stdweb in
  match Dom.Document.get_element_by_id "root" with
  | Some root -> Helix.render root (main ())
  | None -> failwith "no #app"
