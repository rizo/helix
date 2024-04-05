open Stdweb

let ( => ) a b = (a, b)

let make_options ~w () =
  let open Jx.Encoder in
  obj
    [
      "title" => string "Helix - uPlot";
      "width" => int w;
      "height" => int 400;
      "scales" => obj [ "x" => obj [ "time" => bool false ] ];
      "series"
      => array js
           [|
             obj [];
             obj [ "label" => string "One"; "stroke" => string "red" ];
             obj
               [
                 "label" => string "Two";
                 "stroke" => string "blue";
                 "show" => bool false;
               ];
           |];
    ]

let data1 =
  [|
    [| 1.; 2.; 3.; 4.; 5.; 6.; 7. |];
    [| 40.; 43.; 60.; 65.; 71.; 73.; 80. |];
    [| 18.; 24.; 37.; 55.; 55.; 60.; 63. |];
  |]

let data2 =
  [|
    [| 1.; 2.; 3.; 4.; 5.; 6.; 7. |];
    [| 10.; 23.; 10.; 95.; 71.; 93.; 20. |];
    [| 38.; 84.; 77.; 15.; 35.; 10.; 43. |];
  |]

let main () =
  let flag = Signal.make true in
  let uplot = ref None in
  let uplot_node = ref None in
  let adjust_data flag =
    match !uplot with
    | None -> Console.log "uplot not loaded"
    | Some uplot -> Uplot.set_data uplot (if flag then data1 else data2)
  in
  Signal.sub adjust_data flag;

  Dom.Window.bind Dom.Event.resize (fun _ev ->
      match !uplot_node with
      | None -> ()
      | Some node ->
        let w = Dom.Node.get_client_width node in
        Uplot.set_size ~w ~h:400 (Option.get !uplot)
  );

  let open Html in
  div
    [ id "parent" ]
    [
      button
        [ on Dom.Event.click (fun _ -> Signal.update not flag) ]
        [ text "Toggle data" ];
      div
        [
          id "uplot-container";
          style_list [ "width" => "100%"; "border" => "2px solid blue" ];
          Attr.on_mount (fun node ->
              let w = Dom.Node.get_client_width node in
              let options = make_options ~w () in
              uplot := Some (Uplot.make ~options ~data:data1 node);
              uplot_node := Some node
          );
        ]
        [];
    ]

let () =
  match Dom.Document.get_element_by_id "root" with
  | Some root -> Html.mount root (main ())
  | None -> failwith "no #root found"
