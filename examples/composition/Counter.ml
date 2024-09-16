open Helix

let make ?(by = Signal.make 1) lbl =
  let count = Signal.make 0 in

  let html =
    let open Html in
    div []
      [
        span [] [ text lbl ];
        button [ on_click (fun () -> Signal.emit 0 count) ] [ text "Reset" ];
        button
          [ on_click (fun () -> Signal.update (fun n -> n - Signal.get by) count) ]
          [ text "−" ];
        button
          [ on_click (fun () -> Signal.update (fun n -> n + Signal.get by) count) ]
          [ text "+" ];
        span [] [ show int count ];
      ]
  in
  (html, count)
