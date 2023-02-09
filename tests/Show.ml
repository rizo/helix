module Event = Stdweb.Dom.Event
open Helix

let fails () =
  let open Html in
  fragment
    [ h2 [] [ text "h2" ]
    ; pre [] [ code [ class_name "language-ocaml" ] "code" ]
    ; p [] [ text "descr" ]
    ; h3 [] [ text "Example" ]
    ; pre [] [ code [ class_name "language-ocaml" ] "example" ]
    ; empty ()
    ; fragment
        [ h3 [] [ text "Console" ]
        ; div [] [ pre [] [ code [ class_name "plaintext" ] "console" ] ]
        ]
    ]

let switcher () =
  let what = Signal.make `text in
  let open Html in
  div []
    [ div
        [ style [ ("gap", "10px"); ("display", "flex") ] ]
        [ button
            [ on Event.click (fun _ -> Signal.emit `text what) ]
            [ text "text" ]
        ; button
            [ on Event.click (fun _ -> Signal.emit `fragment1 what) ]
            [ text "fragment1" ]
        ; button
            [ on Event.click (fun _ -> Signal.emit `fragment2 what) ]
            [ text "fragment2" ]
        ; button
            [ on Event.click (fun _ -> Signal.emit `div what) ]
            [ text "div" ]
        ; button
            [ on Event.click (fun _ -> Signal.emit `empty1 what) ]
            [ text "empty1" ]
        ; button
            [ on Event.click (fun _ -> Signal.emit `empty2 what) ]
            [ text "empty2" ]
        ; button
            [ on Event.click (fun _ -> Signal.emit `fails1 what) ]
            [ text "fails1" ]
        ; button
            [ on Event.click (fun _ -> Signal.emit `fails2 what) ]
            [ text "fails2" ]
        ]
    ; hr []
    ; what
      |> View.show (fun what ->
             match what with
             | `text -> text "Hello!"
             | `fragment1 ->
               fragment
                 [ div [] [ text "elem 1.1" ]
                 ; div [] [ text "elem 1.2" ]
                 ; div [] [ text "elem 1.3" ]
                 ]
             | `fragment2 ->
               fragment
                 [ div [] [ text "elem 2.1" ]
                 ; div [] [ text "elem 2.2" ]
                 ; div [] [ text "elem 2.3" ]
                 ]
             | `div -> div [] [ text "div 1" ]
             | `empty1 -> empty ()
             | `empty2 -> fragment [ empty () ]
             | `fails1 -> fails ()
             | `fails2 -> fails ())
    ]

let main () = Html.div [] [ switcher () ]
