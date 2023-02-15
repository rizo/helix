# Helix

Build reactive web interfaces in OCaml. 

## Example

```ocaml
open Helix
open Stdweb

let counter () =
  let incr = Signal.make 0 in
  let count = Signal.reduce (fun x y -> x + y) 0 incr in
  let open Html in
  fragment
    [
      h2 [ style [ ("font-family", "monospace") ] ] [ text "Counter" ];
      div [ style [ ("margin-bottom", "20px") ] ] [ text "Compute a count." ];
      div []
        [
          button [ on_click (fun _ -> Signal.emit 1 incr) ] [ text "+" ];
          button [ on_click (fun _ -> Signal.emit (-1) incr) ] [ text "-" ];
          span [ style [ ("margin-left", "5px") ] ] [ View.show int count ];
        ];
    ]

let () =
  match Dom.Document.get_element_by_id "root" with
  | Some root -> Html.render root (counter ())
  | None -> failwith "No #root element found"
```
