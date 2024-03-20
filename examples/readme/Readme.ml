open Helix
open Stdweb

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
            [ on_click (fun _ -> Signal.update (fun n -> n + 1) count) ]
            [ text "+" ];
          button
            [ on_click (fun _ -> Signal.update (fun n -> n - 1) count) ]
            [ text "-" ];
          div
            [
              style_list [ ("font-size", "32px") ];
              bind
                (fun n ->
                  if n < 0 then style_list [ ("color", "red") ]
                  else style_list [ ("color", "blue") ]
                )
                count;
            ]
            [ show (fun n -> text (string_of_int n)) count ];
        ];
    ]

let () =
  match Dom.Document.get_element_by_id "root" with
  | Some root -> Html.mount root (counter ())
  | None -> failwith "No #root element found"
