(* Show fails removing the _same_ element. *)
module Event = Stdweb.Dom.Event
open Helix

let main () =
  let what = Signal.make `view1 in
  let open Html in
  let same = text "hello" in
  div []
    [ div
        [ style [ ("gap", "10px"); ("display", "flex") ] ]
        [ button
            [ on Event.click (fun _ -> Signal.emit `view1 what) ]
            [ text "view1" ]
        ; button
            [ on Event.click (fun _ -> Signal.emit `view2 what) ]
            [ text "view2" ]
        ]
    ; hr []
    ; what
      |> View.show (fun what ->
             match what with
             | `view1 -> same
             | `view2 -> fragment [ same ])
    ]
