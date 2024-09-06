module Document = Stdweb.Dom.Document
open Helix

module Test_01_component = struct
  let component ~label:lbl ~by () =
    let state = Signal.make 0 in
    let html =
      let open Html in
      div []
        [ span [] [ text (lbl ^ ": ") ]
        ; button [ on_click (fun () -> Signal.update (fun n -> n - by) state) ] [ text "-" ]
        ; span [] [ show int state ]
        ; button [ on_click (fun () -> Signal.update (fun n -> n + by) state) ] [ text "+" ]
        ]
    in
    (html, state)

  let make () =
    let html, _ = component ~label:"counter" ~by:1 () in
    Html.div [] [ Html.h2 [] [ Html.text "01 - Components" ]; html ]
end

module Test_02_parallel = struct
  let component ~label:lbl ~by () =
    let state = Signal.make 0 in
    let html =
      let open Html in
      div []
        [ span [] [ text (lbl ^ ": ") ]
        ; button [ on_click (fun () -> Signal.update (fun n -> n - by) state) ] [ text "-" ]
        ; span [] [ show int state ]
        ; button [ on_click (fun () -> Signal.update (fun n -> n + by) state) ] [ text "+" ]
        ]
    in
    (html, state)

  let make () =
    let first, _ = component ~label:"first" ~by:1 () in
    let second, _ = component ~label:"second" ~by:1 () in
    Html.div [] [ Html.h2 [] [ Html.text "02 - Parallel Composition" ]; first; second ]
end

module Test_03_sequential = struct
  let component ~label:lbl ?(by = Signal.make 1) () =
    let state = Signal.make 0 in
    let html =
      let open Html in
      div []
        [ span [] [ text (lbl ^ ": ") ]
        ; button
            [ on_click (fun () -> Signal.update (fun n -> n - Signal.get by) state) ]
            [ text "-" ]
        ; span [] [ show int state ]
        ; button
            [ on_click (fun () -> Signal.update (fun n -> n + Signal.get by) state) ]
            [ text "+" ]
        ]
    in
    (html, state)

  let make () =
    let first, by = component ~label:"first" () in
    let second, _ = component ~label:"second" ~by () in
    Html.div [] [ Html.h2 [] [ Html.text "03 - Sequential" ]; first; second ]
end

module Test_04_multiplicity = struct
  let component ~label:lbl ?(by = Signal.make 1) () =
    let state = Signal.make 0 in
    let html =
      let open Html in
      div []
        [ span [] [ text (lbl ^ ": ") ]
        ; button
            [ on_click (fun () -> Signal.update (fun n -> n - Signal.get by) state) ]
            [ text "-" ]
        ; span [] [ show int state ]
        ; button
            [ on_click (fun () -> Signal.update (fun n -> n + Signal.get by) state) ]
            [ text "+" ]
        ]
    in
    (html, state)

  let make () =
    let counter_view, how_many = component ~label:"how many" () in
    Html.div []
      [ Html.h2 [] [ Html.text "04 - Multiplicity" ]
      ; counter_view
      ; how_many
        |> Signal.map (fun n -> List.init n (fun i -> fst (component ~label:(string_of_int i) ())))
        |> each Fun.id
      ]
end

let main () =
  let open Html in
  div
    [ class_list [ "w-full" ] ]
    [ h1 [] [ text "Composition" ]
    ; blockquote []
        [ text "See: "
        ; a
            [ href "https://github.com/TyOverby/composition-comparison" ]
            [ text "https://github.com/TyOverby/composition-comparison" ]
        ]
    ; hr []
    ; Test_01_component.make ()
    ; hr []
    ; Test_02_parallel.make ()
    ; hr []
    ; Test_03_sequential.make ()
    ; hr []
    ; Test_04_multiplicity.make ()
    ]

let () =
  match Document.get_element_by_id "root" with
  | Some root -> Html.mount root (main ())
  | None -> failwith "No #root element found"
