open Helix
open Stdweb.Dom
open Prelude

let view_flight_booker () =
  let open Html in
  div []
    [
      p [] [ text "Edit device schema." ];
      table
        [ class_list [ "schema_editor" ] ]
        [
          thead []
            [
              tr []
                [
                  th [] [ text "Slot" ];
                  th [] [ text "Name" ];
                  th [] [ text "Type" ];
                  th [] [ text "Actions" ];
                ];
            ];
          tbody []
            [
              tr []
                [
                  td [] [ text "#01" ];
                  td [] [ text "Switch output state" ];
                  td [] [ text "Bool" ];
                  td [] [ button [] [ text "Edit" ] ];
                ];
              tr []
                [
                  td [] [ text "#02" ];
                  td [] [ text "Active power" ];
                  td [] [ text "Number" ];
                  td [] [ button [] [ text "Edit" ] ];
                ];
              tr []
                [
                  td [] [ text "#03" ];
                  td [] [ text "Voltage" ];
                  td [] [ text "Number" ];
                  td [] [ button [] [ text "Edit" ] ];
                ];
              tr []
                [
                  td [] [ text "#04" ];
                  td [] [ text "Total energy consumed" ];
                  td [] [ text "Number" ];
                  td [] [ button [] [ text "Edit" ] ];
                ];
            ];
        ];
    ]

let main () =
  let open Html in
  div
    [ class_list [ "w-full" ] ]
    [ h1 [] [ text "Schema Editor" ]; view_flight_booker () ]

let () =
  match Document.get_element_by_id "root" with
  | Some root -> Html.render root (main ())
  | None -> failwith "No #root element found"
