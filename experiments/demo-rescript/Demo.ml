module Event = Stdweb.Dom.Event
open Stdweb
open Helix

let app () =
  let what = Signal.make `a in
  let open Html in
  div []
    [
      div
        [ style [ ("gap", "10px"); ("display", "flex") ] ]
        [
          button [ on Event.click (fun _ -> Signal.emit `a what) ] [ text "a" ];
          button [ on Event.click (fun _ -> Signal.emit `b what) ] [ text "b" ];
        ];
      hr [];
      what
      |> View.show (fun what ->
             match what with
             | `a -> text "A"
             | `b -> text "B"
         );
    ]

let () =
  match Dom.Document.get_element_by_id "root" with
  | Some root -> Html.mount root (app ())
  | None -> failwith "no #app"
