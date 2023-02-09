open Helix

let main () =
  let open Html in
  div []
    [ h2 [] [ text "Show_empty_bug" ]
    ; Show_empty_bug.main ()
    ; h2 [] [ text "Show" ]
    ; Show.main ()
    ; h2 [] [ text "Conditiona_attr" ] (* ; Conditional_attr.main () *)
    ]

let () =
  let open Stdweb in
  match Dom.Document.get_element_by_id "root" with
  | Some root -> Helix.render root (main ())
  | None -> failwith "no #app"
