open Helix

let counter () =
  let count = Signal.make 0 in
  let open Html in
  div
    [ style_list [ ("border", "1px solid #eee"); ("padding", "1em") ] ]
    [
      h2 [] [ text "Counter" ];
      div [] [ text "Compute a count." ];
      div []
        [
          button
            [ on_click (fun () -> Signal.update (fun n -> n + 1) count) ]
            [ text "+" ];
          button
            [ on_click (fun () -> Signal.update (fun n -> n - 1) count) ]
            [ text "-" ];
          button [ on_click (fun () -> Signal.emit 0 count) ] [ text "Reset" ];
          div
            [
              style_list [ ("font-size", "32px") ];
              bind
                (fun n ->
                  style_list [ ("color", if n < 0 then "red" else "blue") ]
                )
                count;
            ]
            [ show (fun n -> text (string_of_int n)) count ];
        ];
    ]

let () =
  match Stdweb.Dom.Document.get_element_by_id "root" with
  | Some root -> Html.mount root (counter ())
  | None -> failwith "No #root element found"
