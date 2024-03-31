module Ev = Stdweb.Dom.Event
module Node = Stdweb.Dom.Node
open Helix

let test_insert_1 () =
  let n_sig = Signal.make 0 in
  let open Html in
  div
    [ id "root" ]
    [
      ul
        [ style "border: 1px solid cyan" ]
        [
          li [] [ text "static_1" ];
          li [] [ text "static_2" ];
          show (fun n -> li [] [ text ("dynamic_1_" ^ string_of_int n) ]) n_sig;
          li [] [ text "static_3" ];
          show (fun n -> li [] [ text ("dynamic_2_" ^ string_of_int n) ]) n_sig;
          li [] [ text "static_4" ];
          show (fun n -> li [] [ text ("dynamic_3_" ^ string_of_int n) ]) n_sig;
          show (fun n -> li [] [ text ("dynamic_4_" ^ string_of_int n) ]) n_sig;
          li [] [ text "static_5" ];
        ];
      br [];
      button
        [ on_click (fun () -> Signal.update (( + ) 1) n_sig) ]
        [ text "Increment" ];
    ]

let main () =
  let open Html in
  div [] [ h2 [] [ text "test_insert_1" ]; test_insert_1 () ]

let () =
  match Stdweb.Dom.Document.get_element_by_id "root" with
  | Some root -> Html.mount root (main ())
  | None -> failwith "no #app"
