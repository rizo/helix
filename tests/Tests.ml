open Helix

let main () =
  let open Html in
  div [] [ (* Show.main (); *) Conditional.main () (* Each.main () *) ]

let () =
  Test_js.run ();
  Test_signals.run ();
  let open Stdweb in
  match Dom.Document.get_element_by_id "root" with
  | Some root -> Helix.render root (main ())
  | None -> failwith "no #app"
