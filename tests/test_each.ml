module Ev = Stdweb.Dom.Event
open Helix

let test_simple () =
  let open Html in
  ul [] [ Signal.make [ "a"; "b"; "c" ] |> each (fun item -> li [] [ text item ]) ]

let test_simple_same () =
  let open Html in
  ul [] [ Signal.make [ "a"; "a"; "a" ] |> each (fun item -> li [] [ text item ]) ]

let test_swap_1 () =
  let l1 = [ "a"; "b"; "c" ] in
  let l2 = [ "d"; "e"; "f" ] in
  let flag = Signal.make true in
  let open Html in
  div []
    [
      button [ on Ev.click (fun _ -> Signal.update not flag) ] [ text "Swap" ];
      ul []
        [
          flag
          |> Signal.map (fun b -> if b then l1 else l2)
          |> each (fun item -> li [] [ text item ]);
        ];
    ]

let test_swap_2 () =
  let l1 = [ "a" ] in
  let l2 = [ "a"; "a" ] in
  let flag = Signal.make true in
  let open Html in
  div []
    [
      button [ on Ev.click (fun _ -> Signal.update not flag) ] [ text "Swap" ];
      ul []
        [
          flag
          |> Signal.map (fun b -> if b then l1 else l2)
          |> each (fun item -> li [] [ text item ]);
        ];
    ]

let test_swap_3 () =
  let l1 = [ "a"; "a" ] in
  let l2 = [ "a"; "a"; "a" ] in
  let flag = Signal.make true in
  let open Html in
  div []
    [
      button [ on Ev.click (fun _ -> Signal.update not flag) ] [ text "Swap" ];
      ul []
        [
          flag
          |> Signal.map (fun b -> if b then l1 else l2)
          |> each (fun item -> li [] [ text item ]);
        ];
    ]

let test_swap_4 () =
  let l1 = [ "a"; "b"; "c"; "c" ] in
  let l2 = [ "a"; "c"; "d"; "b" ] in
  let flag = Signal.make true in
  let open Html in
  div []
    [
      button [ on Ev.click (fun _ -> Signal.update not flag) ] [ text "Swap" ];
      ul []
        [
          flag
          |> Signal.map (fun b -> if b then l1 else l2)
          |> each (fun item -> li [] [ text item ]);
        ];
    ]

let test_swap_5 () =
  let l1 = [ "a"; "a" ] in
  let l2 = [ "x"; "a"; "a" ] in
  let flag = Signal.make true in
  let open Html in
  div []
    [
      button [ on Ev.click (fun _ -> Signal.update not flag) ] [ text "Swap" ];
      ul []
        [
          flag
          |> Signal.map (fun b -> if b then l1 else l2)
          |> each (fun item -> li [] [ text item ]);
        ];
    ]

let test_swap_6 () =
  let flag = Signal.make true in
  let open Html in
  let l1 = [ "same" ] in
  let l2 = "new" :: l1 in
  div []
    [
      button [ on Ev.click (fun _ -> Signal.update not flag) ] [ text "Swap" ];
      ul []
        [
          flag
          |> Signal.map (fun b -> if b then l1 else l2)
          |> each (fun item -> li [] [ text item ]);
        ];
    ]

let test_swap_7 () =
  let flag = Signal.make true in
  let open Html in
  let l1 = [ input [ placeholder "same" ] ] in
  let l2 = input [ placeholder "new" ] :: l1 in
  div []
    [
      button [ on Ev.click (fun _ -> Signal.update not flag) ] [ text "Swap" ];
      ul []
        [ flag |> Signal.map (fun b -> if b then l1 else l2) |> each (fun item -> li [] [ item ]) ];
    ]

let test_append () =
  let n = ref (-1) in
  let items = Signal.make [] in
  let open Html in
  div []
    [
      button
        [
          on Ev.click (fun _ ->
              Signal.update
                (fun items ->
                  incr n;
                  List.append items [ !n ])
                items);
        ]
        [ text "Add" ];
      button
        [
          on Ev.click (fun _ ->
              n := -1;
              Signal.emit [] items);
        ]
        [ text "Clear" ];
      ul [] [ items |> each (fun item -> li [] [ int item ]) ];
    ]

let test_append_same () =
  let items = Signal.make [ 0 ] in
  let open Html in
  div []
    [
      button
        [ on Ev.click (fun _ -> Signal.update (fun items -> List.append items [ 0 ]) items) ]
        [ text "Add" ];
      ul [] [ items |> each (fun item -> li [] [ int item ]) ];
    ]

let test_conditional_1 () =
  let is_visible = Signal.make true in
  let flag = Signal.make true in
  let l1 = [ "a"; "b"; "c" ] in
  let l2 = [ "b"; "d"; "e" ] in
  let open Html in
  div []
    [
      button [ on Ev.click (fun _ -> Signal.update not is_visible) ] [ text "Toggle show" ];
      button [ on Ev.click (fun _ -> Signal.update not flag) ] [ text "Swap list" ];
      conditional ~on:Fun.id is_visible
        (ul []
           [
             flag
             |> Signal.map (fun b -> if b then l1 else l2)
             |> each (fun item -> li [] [ text item ]);
           ]);
    ]

let test_conditional_2 () =
  let is_visible = Signal.make true in
  let flag = Signal.make false in
  let l1 = [ "a"; "b"; "c" ] in
  let l2 = [ "b"; "d"; "e" ] in
  let open Html in
  div []
    [
      button [ on Ev.click (fun _ -> Signal.update not is_visible) ] [ text "Toggle show" ];
      button [ on Ev.click (fun _ -> Signal.update not flag) ] [ text "Swap list" ];
      conditional ~on:Fun.id is_visible
        (ul []
           [
             flag
             |> Signal.map (fun b -> if b then l1 else l2)
             |> each (fun item -> li [] [ text item ]);
           ]);
    ]

let test_conditional_3 () =
  let is_visible = Signal.make true in
  let items = Signal.make [ "a"; "b"; "X"; "c" ] in
  let open Html in
  div []
    [
      button [ on Ev.click (fun _ -> Signal.update not is_visible) ] [ text "Toggle X" ];
      ul []
        [
          items
          |> each (fun item ->
                 if item = "X" then conditional ~on:Fun.id is_visible (li [] [ text item ])
                 else li [] [ text item ]);
        ];
    ]

let test_show_1 () =
  let count_a = Signal.make 100 in
  let count_b = Signal.make 200 in
  let count_c = Signal.make 300 in
  let items = Signal.make [ `a; `b; `c ] in
  let open Html in
  div []
    [
      button [ on Ev.click (fun _ -> Signal.update (( + ) 1) count_a) ] [ text "Increment a" ];
      button [ on Ev.click (fun _ -> Signal.update (( + ) 1) count_b) ] [ text "Increment b" ];
      button [ on Ev.click (fun _ -> Signal.update (( + ) 1) count_c) ] [ text "Increment c" ];
      ul []
        [
          items
          |> each (function
               | `a -> show (fun n -> li [] [ text "a: "; int n ]) count_a
               | `b -> show (fun n -> li [] [ text "b: "; int n ]) count_b
               | `c -> show (fun n -> li [] [ text "c: "; int n ]) count_c);
        ];
    ]

let test_show_2 () =
  let count_a = Signal.make 100 in
  let count_b = Signal.make 200 in
  let count_c = Signal.make 300 in
  let items = Signal.make [ "a"; "b"; "c" ] in
  let open Html in
  div []
    [
      button
        [
          on_click (fun () ->
              Signal.update
                (fun xs -> if List.length xs = 3 then [ "a"; "c" ] else [ "a"; "b"; "c" ])
                items);
        ]
        [ text "Toggle b" ];
      br [];
      button [ on_click (fun () -> Signal.update (( + ) 1) count_a) ] [ text "Increment a" ];
      button [ on_click (fun () -> Signal.update (( + ) 1) count_b) ] [ text "Increment b" ];
      button [ on_click (fun () -> Signal.update (( + ) 1) count_c) ] [ text "Increment c" ];
      ul []
        [
          items
          |> each (function
               | "a" -> show (fun n -> li [] [ text "a: "; int n ]) count_a
               | "b" -> show ~label:"b" (fun n -> li [] [ text "b: "; int n ]) count_b
               | "c" -> show (fun n -> li [] [ text "c: "; int n ]) count_c
               | _ -> assert false);
        ];
    ]

let test_random () =
  let items = Signal.make (List.init 7 string_of_int) in
  let open Html in
  div []
    [
      button
        [ on Ev.click (fun _ -> Signal.emit (List.init (1 + Random.int 10) string_of_int) items) ]
        [ text "Generate" ];
      ul [] [ items |> each (fun item -> li [] [ text item ]) ];
    ]

let test_interleave () =
  let items = Signal.make (List.init 7 string_of_int) in
  let open Html in
  div []
    [
      button
        [ on Ev.click (fun _ -> Signal.emit (List.init (1 + Random.int 10) string_of_int) items) ]
        [ text "Generate" ];
      div
        [ style_list [ ("display", "flex"); ("flex-direction", "row") ] ]
        [
          ul []
            [ li [] [ Html.text "before 1" ]; each (fun item -> li [] [ Html.text item ]) items ];
          ul [] [ each (fun item -> li [] [ Html.text item ]) items; li [] [ Html.text "after 1" ] ];
          ul []
            [
              li [] [ Html.text "before 1" ];
              each (fun item -> li [] [ Html.text item ]) items;
              li [] [ Html.text "after 1" ];
            ];
          ul []
            [
              li [] [ Html.text "before 1" ];
              each (fun item -> li [] [ Html.text ("1: " ^ item) ]) items;
              each (fun item -> li [] [ Html.text ("2: " ^ item) ]) items;
              li [] [ Html.text "after 1" ];
            ];
          ul []
            [
              each (fun item -> li [] [ Html.text ("1: " ^ item) ]) items;
              li [] [ Html.text "middle 1" ];
              each (fun item -> li [] [ Html.text ("2: " ^ item) ]) items;
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
      h2 [] [ text "swap_5" ];
      test_swap_5 ();
      hr [];
      h2 [] [ text "swap_6" ];
      test_swap_6 ();
      hr [];
      h2 [] [ text "swap_7" ];
      test_swap_7 ();
      hr [];
      h2 [] [ text "conditional_1" ];
      test_conditional_1 ();
      hr [];
      h2 [] [ text "conditional_2" ];
      test_conditional_2 ();
      hr [];
      h2 [] [ text "conditional_3" ];
      test_conditional_3 ();
      hr [];
      h2 [] [ text "show_1" ];
      test_show_1 ();
      hr [];
      h2 [] [ text "show_2" ];
      test_show_2 ();
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

let () =
  match Stdweb.Dom.Document.get_element_by_id "root" with
  | Some root -> Html.mount root (main ())
  | None -> failwith "no #app"
