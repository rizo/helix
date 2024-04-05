module Ev = Stdweb.Dom.Event
module Node = Stdweb.Dom.Node
open Helix

let test_leaky_1 () =
  let n_sig = Signal.make 0 in
  let is_visible = Signal.make true in
  let open Html in
  div
    [ id "main" ]
    [
      show ~label:"baseline"
        (fun n -> text ("baseline: " ^ string_of_int n))
        n_sig;
      br [];
      label []
        [
          text "Active: ";
          input
            [
              type' "checkbox";
              checked (Signal.get is_visible);
              on_checked (fun x -> Signal.emit x is_visible);
            ];
        ];
      br [];
      show ~label:"container"
        (function
          | true ->
            div
              [ id "leaky"; style "border: 1px solid cyan" ]
              [
                show ~label:"leaky"
                  (fun n -> text ("target: " ^ string_of_int n))
                  n_sig;
              ]
          | false -> div [ style "border: 1px solid red" ] [ text "--" ]
          )
        is_visible;
      br [];
      button
        [ on_click (fun () -> Signal.update (( + ) 1) n_sig) ]
        [ text "Increment" ];
    ]

let main () =
  let open Html in
  div [] [ h2 [] [ text "test_leaky_1" ]; test_leaky_1 () ]

let () =
  match Stdweb.Dom.Document.get_element_by_id "root" with
  | Some root -> Html.mount root (main ())
  | None -> failwith "no #app"
