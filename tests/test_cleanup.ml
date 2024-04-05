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

let test_nested_1 () =
  let n1 = Signal.make 100 in
  let n2 = Signal.make 200 in
  let n3 = Signal.make 300 in
  let open Html in
  div
    [ id "main" ]
    [
      show ~label:"n1"
        (fun n1 ->
          div []
            [
              text ("n1: " ^ string_of_int n1);
              show ~label:"n2"
                (fun n2 ->
                  div []
                    [
                      text ("n2: " ^ string_of_int n2);
                      br [];
                      show ~label:"n3"
                        (fun n3 -> text ("n3: " ^ string_of_int n3))
                        n3;
                    ]
                )
                n2;
            ]
        )
        n1;
      button
        [ on_click (fun () -> Signal.update (( + ) 1) n1) ]
        [ text "incr n1" ];
      button
        [ on_click (fun () -> Signal.update (( + ) 1) n2) ]
        [ text "incr n2" ];
      button
        [ on_click (fun () -> Signal.update (( + ) 1) n3) ]
        [ text "incr n3" ];
    ]

let test_nested_2 () =
  let n1 = Signal.make 100 in
  let n2 = Signal.make 200 in
  let n3 = Signal.make 300 in
  let open Html in
  div
    [ id "main" ]
    [
      show ~label:"n1"
        (fun n1 ->
          show ~label:"n2"
            (fun n2 ->
              show ~label:"n3"
                (fun n3 ->
                  text
                    (String.concat ", " (List.map string_of_int [ n1; n2; n3 ]))
                )
                n3
            )
            n2
        )
        n1;
      br [];
      button
        [ on_click (fun () -> Signal.update (( + ) 1) n1) ]
        [ text "incr n1" ];
      button
        [ on_click (fun () -> Signal.update (( + ) 1) n2) ]
        [ text "incr n2" ];
      button
        [ on_click (fun () -> Signal.update (( + ) 1) n3) ]
        [ text "incr n3" ];
    ]

let main () =
  let open Html in
  div []
    [
      h2 [] [ text "test_leaky_1" ];
      test_leaky_1 ();
      br [];
      h2 [] [ text "test_nested_1" ];
      test_nested_1 ();
      br [];
      h2 [] [ text "test_nested_2" ];
      test_nested_2 ();
    ]

let () =
  match Stdweb.Dom.Document.get_element_by_id "root" with
  | Some root -> Html.mount root (main ())
  | None -> failwith "no #app"
