module Event = Stdweb.Dom.Event
open Helix

let test_simple () =
  let open Html in
  ul []
    [
      Signal.make [ "a"; "b"; "c" ]
      |> View.each (fun item -> li [] [ text item ]);
    ]

let test_simple_same () =
  let open Html in
  ul []
    [
      Signal.make [ "a"; "a"; "a" ]
      |> View.each (fun item -> li [] [ text item ]);
    ]

let test_swap_1 () =
  let l1 = [ "a"; "b"; "c" ] in
  let l2 = [ "d"; "e"; "f" ] in
  let flag = Signal.make true in
  let open Html in
  div []
    [
      button [ on_click (fun _ -> Signal.update not flag) ] [ text "Swap" ];
      ul []
        [
          flag
          |> Signal.map (fun b -> if b then l1 else l2)
          |> View.each (fun item -> li [] [ text item ]);
        ];
    ]

let test_swap_2 () =
  let l1 = [ "a" ] in
  let l2 = [ "a"; "a" ] in
  let flag = Signal.make true in
  let open Html in
  div []
    [
      button [ on_click (fun _ -> Signal.update not flag) ] [ text "Swap" ];
      ul []
        [
          flag
          |> Signal.map (fun b -> if b then l1 else l2)
          |> View.each (fun item -> li [] [ text item ]);
        ];
    ]

let test_swap_3 () =
  let l1 = [ "a"; "a" ] in
  let l2 = [ "a"; "a"; "a" ] in
  let flag = Signal.make true in
  let open Html in
  div []
    [
      button [ on_click (fun _ -> Signal.update not flag) ] [ text "Swap" ];
      ul []
        [
          flag
          |> Signal.map (fun b -> if b then l1 else l2)
          |> View.each (fun item -> li [] [ text item ]);
        ];
    ]

let test_swap_4 () =
  let l1 = [ "a"; "b"; "c"; "c" ] in
  let l2 = [ "a"; "c"; "d"; "b" ] in
  let flag = Signal.make true in
  let open Html in
  div []
    [
      button [ on_click (fun _ -> Signal.update not flag) ] [ text "Swap" ];
      ul []
        [
          flag
          |> Signal.map (fun b -> if b then l1 else l2)
          |> View.each (fun item -> li [] [ text item ]);
        ];
    ]

let test_append () =
  let n = ref (-1) in
  let items = Signal.make [] in
  let open Html in
  div []
    [
      button
        [
          on_click (fun _ ->
              Signal.update
                (fun items ->
                  incr n;
                  List.append items [ !n ]
                )
                items
          );
        ]
        [ text "Add" ];
      button
        [
          on_click (fun _ ->
              n := -1;
              Signal.emit [] items
          );
        ]
        [ text "Clear" ];
      ul [] [ items |> View.each (fun item -> li [] [ int item ]) ];
    ]

let test_append_same () =
  let items = Signal.make [ 0 ] in
  let open Html in
  div []
    [
      button
        [
          on_click (fun _ ->
              Signal.update (fun items -> List.append items [ 0 ]) items
          );
        ]
        [ text "Add" ];
      ul [] [ items |> View.each (fun item -> li [] [ int item ]) ];
    ]

let test_conditional_1 () =
  let is_visible = Signal.make true in
  let flag = Signal.make true in
  let l1 = [ "a"; "b"; "c" ] in
  let l2 = [ "b"; "d"; "e" ] in
  let open Html in
  div []
    [
      button
        [ on_click (fun _ -> Signal.update not is_visible) ]
        [ text "Toggle show" ];
      button [ on_click (fun _ -> Signal.update not flag) ] [ text "Swap list" ];
      ul
        [ View.conditional ~on:is_visible ]
        [
          flag
          |> Signal.map (fun b -> if b then l1 else l2)
          |> View.each (fun item -> li [] [ text item ]);
        ];
    ]

(* FIXME *)
let test_conditional_2 () =
  let is_visible = Signal.make true in
  let items = Signal.make [ "a"; "b"; "X"; "c" ] in
  let open Html in
  div []
    [
      button
        [ on_click (fun _ -> Signal.update not is_visible) ]
        [ text "Toggle X" ];
      ul []
        [
          items
          |> View.each (fun item ->
                 li
                   [
                     ( if item = "X" then View.conditional ~on:is_visible
                       else Attr.empty
                     );
                   ]
                   [ text item ]
             );
        ];
    ]

let test_random () =
  let items = Signal.make (List.init 7 string_of_int) in
  let open Html in
  div []
    [
      button
        [
          on_click (fun _ ->
              Signal.emit (List.init (1 + Random.int 10) string_of_int) items
          );
        ]
        [ text "Generate" ];
      ul [] [ items |> View.each (fun item -> li [] [ text item ]) ];
    ]

let test_interleave () =
  let items = Signal.make (List.init 7 string_of_int) in
  let open Html in
  div []
    [
      button
        [
          on_click (fun _ ->
              Signal.emit (List.init (1 + Random.int 10) string_of_int) items
          );
        ]
        [ text "Generate" ];
      div
        [ style [ ("display", "flex"); ("flex-direction", "row") ] ]
        [
          ul []
            [
              li [] [ Html.text "before 1" ];
              View.each (fun item -> li [] [ Html.text item ]) items;
            ];
          ul []
            [
              View.each (fun item -> li [] [ Html.text item ]) items;
              li [] [ Html.text "after 1" ];
            ];
          ul []
            [
              li [] [ Html.text "after 1" ];
              View.each (fun item -> li [] [ Html.text item ]) items;
              li [] [ Html.text "after 1" ];
            ];
          ul []
            [
              li [] [ Html.text "after 1" ];
              View.each (fun item -> li [] [ Html.text ("1: " ^ item) ]) items;
              View.each (fun item -> li [] [ Html.text ("2: " ^ item) ]) items;
              li [] [ Html.text "after 1" ];
            ];
          ul []
            [
              View.each (fun item -> li [] [ Html.text ("1: " ^ item) ]) items;
              li [] [ Html.text "after 1" ];
              View.each (fun item -> li [] [ Html.text ("2: " ^ item) ]) items;
              li [] [ Html.text "after 1" ];
            ];
        ];
    ]

let main () =
  let open Html in
  div []
    [
      h2 [] [ text "simple" ];
      test_simple ();
      hr [];
      h2 [] [ text "simple_same" ];
      test_simple_same ();
      hr [];
      h2 [] [ text "swap_1" ];
      test_swap_1 ();
      hr [];
      h2 [] [ text "swap_2" ];
      test_swap_2 ();
      hr [];
      h2 [] [ text "swap_3" ];
      test_swap_3 ();
      hr [];
      h2 [] [ text "swap_4" ];
      test_swap_4 ();
      hr [];
      h2 [] [ text "conditional_1" ];
      test_conditional_1 ();
      hr [];
      h2 [] [ text "conditional_2" ];
      test_conditional_2 ();
      hr [];
      h2 [] [ text "append" ];
      test_append ();
      h2 [] [ text "append_same" ];
      test_append_same ();
      hr [];
      h2 [] [ text "random" ];
      test_random ();
      hr [];
      h2 [] [ text "interleave" ];
      test_interleave ();
    ]
