module Console = Stdweb.Console
open Helix
open Stdweb.Dom

let bool a b c = if c then a else b
let ( => ) a b = (a, b)

let view_mouse () =
  (* Mouse position at 60fps *)
  let mouse =
    Mouse.position
    |> Signal.sample ~on:(Time.tick ~ms:(1000 / 60))
    |> Signal.map (fun (x, y) ->
           "x = " ^ string_of_float x ^ ", y = " ^ string_of_float y)
  in
  let open Html in
  fragment
    [ h2 [ style [ "font-family" => "monospace" ] ] [ text "Mouse.position" ]
    ; div
        [ style [ "margin-bottom" => "20px" ] ]
        [ text "Render mouse position." ]
    ; View.show text mouse
    ]

let view_timer () =
  let timer = Time.tick ~ms:333 |> Signal.const 1 |> Signal.reduce ( + ) 0 in
  let open Html in
  fragment
    [ h2 [ style [ "font-family" => "monospace" ] ] [ text "Time.tick" ]
    ; div [ style [ "margin-bottom" => "20px" ] ] [ text "Render a timer." ]
    ; View.show int timer
    ]

let view_input_bind () =
  let input_signal = Signal.make "--" in
  let open Html in
  fragment
    [ h2 [ style [ "font-family" => "monospace" ] ] [ text "Html.emit" ]
    ; div
        [ style [ "margin-bottom" => "20px" ] ]
        [ text "Sync input content with two elements." ]
    ; input
        [ placeholder "Type something amazing..."
        ; on_input (fun ev -> Signal.emit (Event.target_value ev) input_signal)
        ]
    ; ul []
        [ li [] [ View.show text input_signal ]
        ; li []
            [ View.show text (input_signal |> Signal.map String.uppercase_ascii)
            ]
        ]
    ]

let view_counter () =
  let incr = Signal.make 0 in
  let count = incr |> Signal.reduce (fun x y -> x + y) 0 in
  let open Html in
  fragment
    [ h2 [ style [ "font-family" => "monospace" ] ] [ text "Html.select" ]
    ; div [ style [ "margin-bottom" => "20px" ] ] [ text "Compute a count." ]
    ; div []
        [ button [ on_click (fun _ -> Signal.emit 1 incr) ] [ text "+" ]
        ; button [ on_click (fun _ -> Signal.emit (-1) incr) ] [ text "-" ]
        ; span [ style [ "margin-left" => "5px" ] ] [ View.show int count ]
        ]
    ]

let view_show () =
  let open Html in
  fragment
    [ h2 [ style [ "font-family" => "monospace" ] ] [ text "Html.show" ]
    ; div
        [ style [ "margin-bottom" => "20px" ] ]
        [ text "Render signal value with function." ]
    ; div [] [ View.show int (Signal.make 5) ]
    ]

let view_toggle () =
  let stylish = Signal.make true in
  let open Html in
  fragment
    [ h2 [ style [ "font-family" => "monospace" ] ] [ text "Html.toggle" ]
    ; div [ style [ "margin-bottom" => "20px" ] ] [ text "show attributes." ]
    ; button
        [ on_click (fun _ -> Signal.update not stylish) ]
        [ text "Style/unstyle element!" ]
    ; div
        [ View.toggle ~on:stylish (style [ "background-color" => "cyan" ]) ]
        [ text "This element has show attributes!" ]
    ]

let view_visibility () =
  let editing_state = Signal.make (false, "Edit me!") in
  let open Html in
  fragment
    [ h2 [ style [ "font-family" => "monospace" ] ] [ text "Html.visible" ]
    ; div
        [ style [ "margin-bottom" => "20px" ] ]
        [ text "Set visibility based on signal value." ]
    ; button
        [ on_click (fun _ ->
              Signal.update (fun (editing, x) -> (not editing, x)) editing_state)
        ]
        [ View.show text
            (Signal.map
               (fun (editing, text) -> if editing then "Save!" else text)
               editing_state)
        ]
    ; input
        [ View.visible ~on:(Signal.map fst editing_state)
        ; style [ "margin-left" => "5px" ]
        ; on_input (fun ev ->
              Signal.update
                (fun (editing, _) -> (editing, Event.target_value ev))
                editing_state)
        ]
    ]

let view_visibility_simple () =
  let is_visible = Signal.make false in
  let open Html in
  fragment
    [ h2 [ style [ "font-family" => "monospace" ] ] [ text "Html.visible" ]
    ; button
        [ on Event.click (fun _ -> Signal.update not is_visible) ]
        [ View.show text (Signal.map (bool "Hide" "Show") is_visible) ]
    ; span [ View.visible ~on:is_visible ] [ text "HELLO" ]
    ]

let view_each () =
  let items =
    Time.tick ~ms:1000
    |> Signal.map (fun () -> List.init (1 + Random.int 10) string_of_int)
  in
  let open Html in
  fragment
    [ h2 [ style [ "font-family" => "monospace" ] ] [ text "Html.each" ]
    ; div [ style [ "margin-bottom" => "20px" ] ] [ text "show lists." ]
    ; ul
        [ style
            [ "outline" => "1px solid pink"
            ; "height" => "200px"
            ; "overflow-y" => "scroll"
            ]
        ]
        [ li [] [ Html.text "fixed li before 1" ]
        ; View.each (fun item -> li [] [ Html.text ("each-1: " ^ item) ]) items
        ; View.each (fun item -> li [] [ Html.text ("each-2: " ^ item) ]) items
        ; li [] [ Html.text "fixed li after 2" ]
        ]
    ]

let main () =
  let open Html in
  div
    [ class_list [ "w-full" ] ]
    [ h1 [] [ text "Helix Demo" ]
    ; view_mouse ()
    ; view_visibility_simple ()
    ; view_timer ()
    ; view_input_bind ()
    ; view_counter ()
    ; view_visibility ()
    ; view_show ()
    ; view_toggle ()
    ; view_each ()
    ]

let () =
  match Document.get_element_by_id "root" with
  | Some root -> Html.render root (main ())
  | None -> failwith "No #root element found"
