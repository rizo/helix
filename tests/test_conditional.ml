module Ev = Stdweb.Dom.Event
open Helix

let test_simple () =
  let open Html in
  div []
    [
      conditional ~on:Fun.id (Signal.make true) (div [] [ text "present" ]);
      conditional ~on:Fun.id (Signal.make false) (footer [] [ text "missing" ]);
    ]

let test_toggle_simple () =
  let is_present = Signal.make true in
  let open Html in
  div []
    [
      button [ on Ev.click (fun _ -> Signal.update not is_present) ] [ text "Toggle present" ];
      ul [] [ li [] [ conditional ~on:Fun.id is_present (span [] [ text "HELLO" ]) ] ];
    ]

let test_toggle_siblings () =
  let hello = Signal.make true in
  let bye = Signal.make false in
  let open Html in
  div []
    [
      button [ on Ev.click (fun _ -> Signal.update not hello) ] [ text "Toggle HELLO" ];
      button [ on Ev.click (fun _ -> Signal.update not bye) ] [ text "Toggle BYE" ];
      button
        [
          on Ev.click (fun _ ->
              Signal.update not hello;
              Signal.update not bye);
        ]
        [ text "Toggle BOTH" ];
      ul []
        [
          li [] [ conditional ~on:Fun.id hello (span [] [ text "HELLO 1" ]) ];
          li [] [ conditional ~on:Fun.id hello (span [] [ text "HELLO 2" ]) ];
        ];
      ul []
        [
          li [] [ span [] [ text "before 1" ] ];
          li [] [ span [] [ text "before 2" ] ];
          li [] [ conditional ~on:Fun.id hello (span [] [ text "HELLO" ]) ];
          li [] [ span [] [ text "after 1" ] ];
          li [] [ span [] [ text "after 2" ] ];
        ];
      ul []
        [
          li [] [ span [] [ text "before 1" ] ];
          li [] [ span [] [ text "before 2" ] ];
          li [] [ conditional ~on:Fun.id hello (span [] [ text "HELLO 1" ]) ];
          li [] [ conditional ~on:Fun.id hello (span [] [ text "HELLO 2" ]) ];
          li [] [ span [] [ text "after 1" ] ];
          li [] [ span [] [ text "after 2" ] ];
        ];
      ul []
        [
          li [] [ span [] [ text "before 1" ] ];
          li [] [ conditional ~on:Fun.id hello (span [] [ text "HELLO 1" ]) ];
        ];
      ul []
        [
          li [] [ conditional ~on:Fun.id hello (span [] [ text "HELLO 1" ]) ];
          li [] [ span [] [ text "after 1" ] ];
        ];
      ul []
        [
          li [] [ conditional ~on:Fun.id hello (span [] [ text "HELLO" ]) ];
          li [] [ conditional ~on:Fun.id bye (span [] [ text "BYE" ]) ];
        ];
      ul []
        [
          li [] [ span [] [ text "before 1" ] ];
          li [] [ conditional ~on:Fun.id bye (span [] [ text "BYE 1" ]) ];
          li [] [ conditional ~on:Fun.id hello (span [] [ text "HELLO 1" ]) ];
          li [] [ conditional ~on:Fun.id hello (span [] [ text "HELLO 2" ]) ];
          li [] [ conditional ~on:Fun.id bye (span [] [ text "BYE 2" ]) ];
          li [] [ span [] [ text "after 1" ] ];
        ];
    ]

let main () =
  let open Html in
  div []
    [
      h2 [] [ text "test_simple" ];
      test_simple ();
      hr [];
      h2 [] [ text "test_togglsimple" ];
      test_toggle_simple ();
      hr [];
      h2 [] [ text "test_toggle_siblings" ];
      test_toggle_siblings ();
    ]

let () =
  Helix.enable_debug false;
  match Stdweb.Dom.Document.get_element_by_id "root" with
  | Some root -> Html.mount root (main ())
  | None -> failwith "no #app"
