module Console = Stdweb.Console
open Helix
open Stdweb.Dom

let bool a b c = if c then a else b
let ( => ) a b = (a, b)

let view_mouse () =
  (* Mouse position at 60fps *)
  let mouse =
    Mouse.position ()
    |> Signal.sample ~on:(Time.tick ~ms:(1000 / 60))
    |> Signal.map (fun (x, y) ->
           "x = " ^ string_of_float x ^ ", y = " ^ string_of_float y
       )
  in
  let open Html in
  fragment
    [
      h2
        [ style_list [ "font-family" => "monospace" ] ]
        [ text "Mouse.position" ];
      div
        [ style_list [ "margin-bottom" => "20px" ] ]
        [ text "Render mouse position." ];
      show text mouse;
    ]

let view_timer () =
  (* [TODO] pass custom equal to prevent dedup *)
  (* let timer = Time.tick ~ms:333 |> Signal.const 1 |> Signal.reduce ( + ) 0 in *)
  let timer = Time.tick ~ms:333 |> Signal.reduce (fun t () -> t + 1) 0 in
  let open Html in
  fragment
    [
      h2 [ style_list [ "font-family" => "monospace" ] ] [ text "Time.tick" ];
      div
        [ style_list [ "margin-bottom" => "20px" ] ]
        [ text "Render a timer." ];
      show int timer;
    ]

let view_input_bind () =
  let input_signal = Signal.make "--" in
  let open Html in
  fragment
    [
      h2 [ style_list [ "font-family" => "monospace" ] ] [ text "Html.emit" ];
      div
        [ style_list [ "margin-bottom" => "20px" ] ]
        [ text "Sync input content with two elements." ];
      input
        [
          placeholder "Type something amazing...";
          on Event.input (fun ev ->
              Signal.emit (ev |> Event.target |> Node.get_value) input_signal
          );
        ];
      ul []
        [
          li [] [ show text input_signal ];
          li []
            [ show text (input_signal |> Signal.map String.uppercase_ascii) ];
        ];
    ]

let view_counter () =
  let count = Signal.make 0 in
  let open Html in
  fragment
    [
      h2 [ style_list [ "font-family" => "monospace" ] ] [ text "counter" ];
      div
        [ style_list [ "margin-bottom" => "20px" ] ]
        [ text "Compute a count." ];
      div []
        [
          button
            [ on Event.click (fun _ -> Signal.update (fun n -> n + 1) count) ]
            [ text "+" ];
          button
            [ on Event.click (fun _ -> Signal.update (fun n -> n - 1) count) ]
            [ text "-" ];
          span [ style_list [ "margin-left" => "5px" ] ] [ show int count ];
        ];
    ]

let view_show () =
  let open Html in
  fragment
    [
      h2 [ style_list [ "font-family" => "monospace" ] ] [ text "Html.show" ];
      div
        [ style_list [ "margin-bottom" => "20px" ] ]
        [ text "Render signal value with function." ];
      div [] [ show int (Signal.make 5) ];
    ]

let view_toggle () =
  let stylish = Signal.make true in
  let open Html in
  fragment
    [
      h2 [ style_list [ "font-family" => "monospace" ] ] [ text "Html.toggle" ];
      div
        [ style_list [ "margin-bottom" => "20px" ] ]
        [ text "show attributes." ];
      button
        [ on Event.click (fun _ -> Signal.update not stylish) ]
        [ text "Style/unstyle element!" ];
      div
        [
          toggle ~on:Fun.id
            (style_list [ "background-color" => "cyan" ])
            stylish;
        ]
        [ text "This element has show attributes!" ];
    ]

let view_visibility () =
  let editing_state = Signal.make (false, "Edit me!") in
  let open Html in
  fragment
    [
      h2 [ style_list [ "font-family" => "monospace" ] ] [ text "Html.visible" ];
      div
        [ style_list [ "margin-bottom" => "20px" ] ]
        [ text "Set visibility based on signal value." ];
      button
        [
          on Event.click (fun _ ->
              Signal.update (fun (editing, x) -> (not editing, x)) editing_state
          );
        ]
        [
          show text
            (Signal.map
               (fun (editing, text) -> if editing then "Save!" else text)
               editing_state
            );
        ];
      input
        [
          visible ~on:(Signal.map fst editing_state);
          style_list [ "margin-left" => "5px" ];
          on Event.input (fun ev ->
              let target = Event.target ev in
              Signal.update
                (fun (editing, _) -> (editing, Node.get_value target))
                editing_state
          );
        ];
    ]

let view_visibility_simple () =
  let is_visible = Signal.make false in
  let open Html in
  fragment
    [
      h2 [ style_list [ "font-family" => "monospace" ] ] [ text "Html.visible" ];
      button
        [ on Event.click (fun _ -> Signal.update not is_visible) ]
        [ show text (Signal.map (bool "Hide" "Show") is_visible) ];
      span [ visible ~on:is_visible ] [ text "HELLO" ];
    ]

let view_each () =
  let items =
    Time.tick ~ms:1000
    |> Signal.map (fun () -> List.init (1 + Random.int 10) string_of_int)
  in
  let open Html in
  fragment
    [
      h2 [ style_list [ "font-family" => "monospace" ] ] [ text "Html.each" ];
      div [ style_list [ "margin-bottom" => "20px" ] ] [ text "show lists." ];
      ul
        [
          style_list
            [
              "outline" => "1px solid pink";
              "height" => "200px";
              "overflow-y" => "scroll";
            ];
        ]
        [
          li [] [ Html.text "fixed li before 1" ];
          each (fun item -> li [] [ Html.text ("each-1: " ^ item) ]) items;
          each (fun item -> li [] [ Html.text ("each-2: " ^ item) ]) items;
          li [] [ Html.text "fixed li after 2" ];
        ];
    ]

let main () =
  let open Html in
  div
    [ class_list [ "w-full" ] ]
    [
      h1 [] [ text "Helix Demo" ];
      view_mouse ();
      view_visibility_simple ();
      view_timer ();
      view_input_bind ();
      view_counter ();
      view_visibility ();
      view_show ();
      view_toggle ();
      view_each ();
    ]

let () =
  Helix.enable_debug true;
  match Document.get_element_by_id "root" with
  | Some root -> Html.mount root (main ())
  | None -> failwith "No #root element found"
