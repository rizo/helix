module Document = Stdweb.Dom.Document
open Helix

module Counter = struct
  let make ~label:lbl ?(init = 0) ?(by = Signal.make 1) () =
    let state = Signal.make init in
    let html =
      let open Html in
      div
        [ style_list [ ("display", "flex"); ("gap", "5px"); ("align-items", "center") ] ]
        [
          span
            [ style_list [ ("display", "inline-block"); ("width", "100px") ] ]
            [ text (lbl ^ ": ") ];
          button
            [ on_click (fun () -> Signal.update (fun n -> n - Signal.get by) state) ]
            [ text "-" ];
          button
            [ on_click (fun () -> Signal.update (fun n -> n + Signal.get by) state) ]
            [ text "+" ];
          span [] [ show int state ];
        ]
    in
    (html, state)
end

module Test_01_component = struct
  let make () =
    let html, _ = Counter.make ~label:"counter" () in
    let open Html in
    fieldset [] [ legend [] [ h2 [] [ text "01. Single" ] ]; html ]
end

module Test_02_parallel = struct
  let make () =
    let first, _ = Counter.make ~label:"first" () in
    let second, _ = Counter.make ~label:"second" () in
    let open Html in
    fieldset
      [ style_list [ ("display", "flex"); ("flex-direction", "column"); ("gap", "5px") ] ]
      [ legend [] [ h2 [] [ text "02. Parallel" ] ]; first; second ]
end

module Test_03_sequential = struct
  let make () =
    let first, by = Counter.make ~label:"first" () in
    let second, _ = Counter.make ~label:"second" ~by () in
    let open Html in
    fieldset
      [ style_list [ ("display", "flex"); ("flex-direction", "column"); ("gap", "5px") ] ]
      [ legend [] [ h2 [] [ text "03. Sequential" ] ]; first; second ]
end

module Test_04_multiplicity = struct
  let make () =
    let counter_view, how_many = Counter.make ~label:"how many" () in
    let open Html in
    fieldset
      [ style_list [ ("display", "flex"); ("flex-direction", "column"); ("gap", "5px") ] ]
      [
        legend [] [ h2 [] [ text "04. Multiplicity" ] ];
        counter_view;
        how_many
        |> Signal.map (fun n -> List.init n (fun i -> string_of_int i))
        |> each (fun label -> fst (Counter.make ~label ()));
      ]
end

module Test_05_inception = struct
  let make () =
    let counter_view, how_many = Counter.make ~label:"how deep" () in

    (* Compute add/delete deltas from the counter signal *)
    let deltas =
      Signal.reduce (fun (n, _) n' -> (n', n' - n > 0)) (Signal.get how_many, false) how_many
    in

    let items =
      deltas
      |> Signal.reduce
           (fun acc (n, delta) ->
             let label = string_of_int n in
             match (delta, acc) with
             | true, [] ->
               let html, state = Counter.make ~label () in
               [ (label, html, state) ]
             | true, (_, _, prev_state) :: _ ->
               let html, state = Counter.make ~label ~by:prev_state () in
               (label, html, state) :: acc
             | false, [] -> []
             | false, _ -> List.tl acc)
           []
      |> Signal.map (fun acc -> List.rev acc)
    in

    let open Html in
    fieldset
      [ style_list [ ("display", "flex"); ("flex-direction", "column"); ("gap", "5px") ] ]
      [
        legend [] [ h2 [] [ text "05. Inception" ] ];
        counter_view;
        each ~key:(fun (lbl, _, _) -> lbl) (fun (_, html, _) -> html) items;
      ]
end

let main () =
  let open Html in
  div []
    [
      h1 [] [ text "Component composition" ];
      blockquote []
        [
          text "See: ";
          a
            [ href "https://github.com/TyOverby/composition-comparison" ]
            [ text "https://github.com/TyOverby/composition-comparison" ];
        ];
      section
        [ style_list [ ("display", "flex"); ("flex-direction", "column"); ("gap", "45px") ] ]
        [
          Test_01_component.make ();
          Test_02_parallel.make ();
          Test_03_sequential.make ();
          Test_04_multiplicity.make ();
          Test_05_inception.make ();
        ];
    ]

let () =
  match Document.get_element_by_id "root" with
  | Some root -> Html.mount root (main ())
  | None -> failwith "No #root element found"
