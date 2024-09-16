open Helix
open Stdweb.Dom

let ( => ) a b = (a, b)
let ( >> ) g f x = f (g x)

let view_counter () =
  let count = Signal.make 0 in
  let open Html in
  fieldset []
    [
      legend [] [ h2 [] [ text "Counter" ] ];
      div
        [ style_list [ "margin-bottom" => "20px" ] ]
        [ text "Increment or decrement a number by 1." ];
      div []
        [
          button [ on Event.click (fun _ -> Signal.update (fun n -> n + 1) count) ] [ text "+" ];
          button [ on Event.click (fun _ -> Signal.update (fun n -> n - 1) count) ] [ text "-" ];
          span
            [
              bind (fun n -> if n < 0 then style_list [ ("color", "red") ] else Attr.nop) count;
              style_list [ "margin-left" => "5px" ];
            ]
            [ show int count ];
        ];
    ]

let view_temp_conv () =
  let f_of_c c = (c *. 9. /. 5.) +. 32. in
  let c_of_f f = (f -. 32.) *. 5. /. 9. in
  let c_signal = Signal.make "" in
  let f_signal = Signal.make "" in
  let on_temp_input conv signal ev =
    let value = Node.get_value (Event.target ev) in
    let value' = try value |> float_of_string |> conv |> string_of_float with Failure _ -> "" in
    Signal.emit value' signal
  in
  let open Html in
  fieldset []
    [
      legend [] [ h2 [] [ text "Temperature Converter" ] ];
      div
        [ style_list [ "margin-bottom" => "20px" ] ]
        [ text "Bidirectional temperature converter." ];
      input [ bind value c_signal; on Event.input (on_temp_input f_of_c f_signal) ];
      text " Celsius = ";
      input [ bind value f_signal; on Event.input (on_temp_input c_of_f c_signal) ];
      text " Fahrenheit";
    ]

let view_flight_booker () =
  let is_valid_date str = String.length str = 10 in
  let is_valid_book (d1, d2) ft =
    (String.equal "oneway" ft && is_valid_date d1)
    || (String.equal "return" ft && is_valid_date d1 && is_valid_date d2 && d2 >= d1)
  in
  let flight_type = Signal.make "oneway" in
  let dates = Signal.make ("2023-01-01", "2023-01-01") in
  let msg_signal = Signal.make "" in
  let click_submit () =
    let d1, d2 = Signal.get dates in
    let ft = Signal.get flight_type in
    let msg =
      String.concat " "
        (if String.equal ft "oneway" then [ "You have booked a one-way flight on"; d1 ]
         else [ "You have booked a return flight on"; d1; "and"; d2 ])
    in
    Signal.emit msg msg_signal
  in
  let open Html in
  fieldset []
    [
      legend [] [ h2 [] [ text "Flight Booker" ] ];
      div [ style_list [ "margin-bottom" => "20px" ] ] [ text "Demonstrates constraints." ];
      div
        [
          style_list
            [
              "display" => "flex"; "gap" => "10px"; "flex-direction" => "column"; "width" => "200px";
            ];
        ]
        [
          select
            [
              name "flight_type";
              on_change (fun value ->
                  Signal.emit "" msg_signal;
                  Signal.emit value flight_type);
            ]
            [
              option [ value "oneway" ] [ text "one-way flight" ];
              option [ value "return" ] [ text "return flight" ];
            ];
          input
            [
              placeholder "YYYY-MM-DD";
              value (fst (Signal.get dates));
              on_input (fun value -> Signal.update (fun (_, d2) -> (value, d2)) dates);
              toggle
                ~on:(fun (d1, _) -> not (is_valid_date d1))
                dates
                (style_list [ "outline" => "1px solid red" ]);
            ];
          input
            [
              placeholder "YYYY-MM-DD";
              value (snd (Signal.get dates));
              on_input (fun value -> Signal.update (fun (d1, _) -> (d1, value)) dates);
              toggle ~on:(String.equal "oneway") flight_type (disabled true);
              toggle
                ~on:(fun ((_, d2), ft) -> String.equal "return" ft && not (is_valid_date d2))
                (Signal.pair dates flight_type)
                (style_list [ "outline" => "1px solid red" ]);
            ];
          button
            [
              on_click click_submit;
              toggle
                ~on:(fun (dates, ft) -> not (is_valid_book dates ft))
                (Signal.pair dates flight_type) (disabled true);
            ]
            [ text "Book" ];
          show text msg_signal;
        ];
    ]

(* [TODO] Incomplete impl. *)
let view_timer () =
  let open Html in
  fieldset []
    [
      legend [] [ h2 [] [ text "Timer" ] ];
      div [ style_list [ "margin-bottom" => "20px" ] ] [ text "Concurrency." ];
      div
        [
          style_list
            [
              "display" => "flex"; "gap" => "10px"; "flex-direction" => "column"; "width" => "350px";
            ];
        ]
        [
          div []
            [
              text "Elapsed time: ";
              progress [ style_list [ "width" => "100%" ]; attr "max" "100"; value "70" ] [];
              text "10s";
            ];
          div
            [ style_list [ "border" => "1px" ] ]
            [
              label [ for' "duration" ] [ text "Duration:" ];
              input
                [
                  name "duration";
                  type' "range";
                  attr "min" "0";
                  attr "max" "100";
                  value "10";
                  attr "step" "1";
                  style_list [ "width" => "100%" ];
                ];
            ];
          button [] [ text "Reset" ];
        ];
    ]

let main () =
  let open Html in
  div []
    [
      h1 [] [ text "Helix 7 GUIs" ];
      blockquote []
        [
          text "See: ";
          a
            [ href "https://eugenkiss.github.io/7guis/tasks" ]
            [ text "https://eugenkiss.github.io/7guis/tasks" ];
        ];
      view_counter ();
      view_temp_conv ();
      view_flight_booker ();
      view_timer ();
    ]

let () =
  match Document.get_element_by_id "root" with
  | Some root -> Html.mount root (main ())
  | None -> failwith "No #root element found"
