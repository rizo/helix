# Helix

## Example

```ocaml
let view_counter () =
  let incr = Signal.make 0 in
  let count = incr |> Signal.reduce (fun x y -> x + y) 0 in
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
```
