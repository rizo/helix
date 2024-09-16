open Helix

(* Single *)

module Test_01_component = struct
  let make () =
    let html, _ = Counter.make "counter" in
    let open Html in
    fieldset [] [ legend [] [ h2 [] [ text "01. Single" ] ]; html ]
end

(* Parallel *)

module Test_02_parallel = struct
  let make () =
    let first, _ = Counter.make "first" in
    let second, _ = Counter.make "second" in
    let open Html in
    fieldset [] [ legend [] [ h2 [] [ text "02. Parallel" ] ]; first; second ]
end

(* Sequential *)

module Test_03_sequential = struct
  let make () =
    let first, count = Counter.make "first" in
    let second, _ = Counter.make ~by:count "second" in
    let open Html in
    fieldset [] [ legend [] [ h2 [] [ text "03. Sequential" ] ]; first; second ]
end

(* Multiplicity *)

module Test_04_multiplicity = struct
  let make () =
    let count_html, how_many = Counter.make "how many" in
    let open Html in
    fieldset []
      [
        legend [] [ h2 [] [ text "04. Multiplicity" ] ];
        count_html;
        how_many
        |> Signal.map (fun n -> List.init n (fun i -> string_of_int i))
        |> each (fun label -> fst (Counter.make label));
      ]
end

(* Inception *)

module Test_05_inception = struct
  let make () =
    let counter_view, how_many = Counter.make "how deep" in

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
               let html, state = Counter.make label in
               [ (label, html, state) ]
             | true, (_, _, prev_state) :: _ ->
               let html, state = Counter.make ~by:prev_state label in
               (label, html, state) :: acc
             | false, [] -> []
             | false, _ -> List.tl acc)
           []
      |> Signal.map (fun acc -> List.rev acc)
    in

    let open Html in
    fieldset []
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
      h1 [] [ text "Composition demo" ];
      p []
        [
          a
            [ href "https://github.com/TyOverby/composition-comparison" ]
            [ text "https://github.com/TyOverby/composition-comparison" ];
        ];
      section
        [ id "main" ]
        [
          Test_01_component.make ();
          (* Test_02_parallel.make (); *)
          (* Test_03_sequential.make (); *)
          (* Test_04_multiplicity.make (); *)
          (* Test_05_inception.make (); *)
        ];
    ]

let () =
  match Stdweb.Dom.Document.get_element_by_id "root" with
  | Some root -> Html.mount root (main ())
  | None -> failwith "No #root element found"
