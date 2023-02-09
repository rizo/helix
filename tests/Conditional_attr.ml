open Helix

let main () =
  let is_present = Signal.make true in
  let open Html in
  div []
    [ div
        [ style [ ("gap", "10px"); ("display", "flex") ] ]
        [ button
            [ on_click (fun _ -> Signal.update not is_present) ]
            [ text "Toggle present" ]
        ]
    ; hr []
    ; div [ View.conditional_attr is_present ] [ text "HELLO" ]
    ]
