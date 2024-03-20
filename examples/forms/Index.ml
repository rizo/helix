open Stdweb.Dom

let view_flight_booker () =
  let is_valid_date str = String.length str = 10 in
  let _is_valid_book (d1, d2) ft =
    (String.equal "oneway" ft && is_valid_date d1)
    || String.equal "return" ft
       && is_valid_date d1
       && is_valid_date d2
       && d2 >= d1
  in
  let flight_type = Signal.make "oneway" in
  let dates = Signal.make ("2023-01-01", "2023-01-01") in
  let msg_signal = Signal.make "" in
  let _click_submit _ =
    let d1, d2 = Signal.get dates in
    let ft = Signal.get flight_type in
    let msg =
      String.concat " "
        ( if String.equal ft "oneway" then
          [ "You have booked a one-way flight on"; d1 ]
        else [ "You have booked a return flight on"; d1; "and"; d2 ]
        )
    in
    Signal.emit msg msg_signal
  in
  let open Html in
  div
    [ class_list [ "w-full" ] ]
    [
      p [] [ text "Demonstrates constraints." ];
      div
        [ class_list (Styles.row ~stretch:true ~distribute:`between ()) ]
        [ text "#"; text "Name" ];
    ]

let main () =
  let open Html in
  div
    [ class_list [ "w-full" ] ]
    [ h1 [] [ text "Hyper Schema Editor" ]; view_flight_booker () ]

let () =
  match Document.get_element_by_id "root" with
  | Some root -> Html.mount root (main ())
  | None -> failwith "No #root element found"
