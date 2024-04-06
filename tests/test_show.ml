module Ev = Stdweb.Dom.Event
open Helix

let test_simple () =
  let open Html in
  ul []
    [
      li [] [ show Html.text (Signal.make "text") ];
      li [] [ show Html.int (Signal.make 42) ];
      li [] [ show (fun () -> Html.empty) (Signal.make ()) ];
      li []
        [
          show
            (fun (num, msg) ->
              Html.span [] [ text msg; text (string_of_int num) ]
            )
            (Signal.make (42, "hello"));
        ];
    ]

let test_updates () =
  let number = Signal.make 0 in
  let open Html in
  div []
    [
      button
        [ on Ev.click (fun _ -> Signal.update (( + ) 1) number) ]
        [ text "incr" ];
      show int number;
    ]

let test_nested () =
  let open Html in
  let number = Signal.make 0 in
  div []
    [
      div [] [ show (fun x -> show text (Signal.make x)) (Signal.make "hello") ];
      div []
        [
          button
            [ on Ev.click (fun _ -> Signal.update (( + ) 1) number) ]
            [ text "incr" ];
          show (fun x -> show int (Signal.make x)) number;
        ];
      div []
        [
          button
            [ on Ev.click (fun _ -> Signal.update (( + ) 1) number) ]
            [ text "incr" ];
          show
            (fun x ->
              if x mod 2 = 0 then text "NOP" else show int (Signal.make x)
            )
            number;
        ];
    ]

let test_nested_bug_1 () =
  let signal = Signal.make 0 in
  let open Html in
  div []
    [
      show ~label:"outer"
        (fun x1 ->
          show ~label:"inner"
            (fun x2 ->
              text (String.concat "+" [ string_of_int x1; string_of_int x2 ])
            )
            signal
        )
        signal;
      br [];
      button
        [ on Ev.click (fun _ -> Signal.update (( + ) 1) signal) ]
        [ text "Trigger nested bug 1" ];
    ]

let test_switcher () =
  let complex () =
    let open Html in
    fragment
      [
        h2 [] [ text "h2" ];
        pre [] [ code [ class_name "language-ocaml" ] [ text "code" ] ];
        p [] [ text "descr" ];
        h3 [] [ text "Example" ];
        pre [] [ code [ class_name "language-ocaml" ] [ text "example" ] ];
        empty;
        fragment
          [
            h3 [] [ text "Console" ];
            div []
              [ pre [] [ code [ class_name "plaintext" ] [ text "console" ] ] ];
          ];
      ]
  in
  let what = Signal.make `text in
  let open Html in
  div []
    [
      div
        [ style_list [ ("gap", "10px"); ("display", "flex") ] ]
        [
          button
            [ on Ev.click (fun _ -> Signal.emit `text what) ]
            [ text "text" ];
          button
            [ on Ev.click (fun _ -> Signal.emit `fragment1 what) ]
            [ text "fragment1" ];
          button
            [ on Ev.click (fun _ -> Signal.emit `fragment2 what) ]
            [ text "fragment2" ];
          button [ on Ev.click (fun _ -> Signal.emit `div what) ] [ text "div" ];
          button
            [ on Ev.click (fun _ -> Signal.emit `empty1 what) ]
            [ text "empty1" ];
          button
            [ on Ev.click (fun _ -> Signal.emit `empty2 what) ]
            [ text "empty2" ];
          button
            [ on Ev.click (fun _ -> Signal.emit `complex1 what) ]
            [ text "complex1" ];
          button
            [ on Ev.click (fun _ -> Signal.emit `complex2 what) ]
            [ text "complex2" ];
        ];
      what
      |> show (fun what ->
             match what with
             | `text -> text "Hello!"
             | `fragment1 ->
               fragment
                 [
                   div [] [ text "elem 1.1" ];
                   div [] [ text "elem 1.2" ];
                   div [] [ text "elem 1.3" ];
                 ]
             | `fragment2 ->
               fragment
                 [
                   div [] [ text "elem 2.1" ];
                   div [] [ text "elem 2.2" ];
                   div [] [ text "elem 2.3" ];
                 ]
             | `div -> div [] [ text "div 1" ]
             | `empty1 -> empty
             | `empty2 -> fragment [ empty ]
             | `complex1 -> complex ()
             | `complex2 -> complex ()
         );
    ]

let main () =
  let open Html in
  div []
    [
      h2 [] [ text "Show/simple" ];
      test_simple ();
      hr [];
      h2 [] [ text "Show/updates" ];
      test_updates ();
      hr [];
      h2 [] [ text "Show/nested" ];
      test_nested ();
      hr [];
      h2 [] [ text "Show/switcher" ];
      test_switcher ();
      h2 [] [ text "Show/nested_bug_1" ];
      test_nested_bug_1 ();
    ]

let () =
  Helix.enable_debug true;
  match Stdweb.Dom.Document.get_element_by_id "root" with
  | Some root -> Html.mount root (main ())
  | None -> failwith "no #app"
