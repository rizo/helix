module Event = Stdweb.Dom.Event
module Node = Stdweb.Dom.Node
open Helix

let main () =
  let todos = Signal.make Todos.empty in
  let remaining = Signal.map Todos.count_remaining todos in
  let on_todo_input ev =
    let key = Event.key ev in
    let target = Event.target ev in
    let title = Node.get_value target in
    if String.equal "Enter" key && String.length title > 0 then (
      Signal.update (Todos.add title) todos;
      Node.set_value target "")
  in
  let open Html in
  section
    [ class_list [ "todoapp" ] ]
    [
      header
        [ class_list [ "header" ] ]
        [
          h1 [] [ text "todos" ];
          input
            [
              class_name "new-todo";
              autofocus true;
              placeholder "What is to be done?";
              on Event.keydown on_todo_input;
            ];
        ];
      section
        [ class_list [ "main" ] ]
        [
          input [ id "toggle-all"; type' "checkbox"; class_list [ "toggle-all" ] ];
          label [ for' "toggle-all" ] [ text "Toggle all" ];
          ul
            [ class_name "todo-list" ]
            [
              todos
              |> Signal.map Todos.filtered
              |> each (fun (todo_id, { Todos.title; completed }) ->
                     li
                       [ class_name "todo" ]
                       [
                         div
                           [ class_name "view" ]
                           [
                             input
                               [
                                 class_name "toggle";
                                 type' "checkbox";
                                 checked completed;
                                 on_click (fun () -> Signal.update (Todos.toggle todo_id) todos);
                               ];
                             label [] [ text title ];
                             button
                               [
                                 class_name "destroy";
                                 on_click (fun () -> Signal.update (Todos.remove todo_id) todos);
                               ]
                               [];
                           ];
                       ]);
            ];
        ]
      |> conditional ~on:(fun todos -> Todos.length todos > 0) todos;
      footer
        [ class_name "footer" ]
        [
          span
            [ class_name "todo-count" ]
            [
              strong []
                [
                  (let$ n = remaining in
                   [ string_of_int n; (if n = 1 then "item" else "items"); "left" ]
                   |> String.concat " "
                   |> text);
                ];
            ];
          ul
            [ class_name "filters" ]
            [
              li []
                [
                  a [ on_click (fun () -> Signal.update (Todos.filter `all) todos) ] [ text "All" ];
                ];
              li []
                [
                  a
                    [ on_click (fun () -> Signal.update (Todos.filter `remaining) todos) ]
                    [ text "Remaining" ];
                ];
              li []
                [
                  a
                    [ on_click (fun () -> Signal.update (Todos.filter `completed) todos) ]
                    [ text "Completed" ];
                ];
            ];
          button
            [
              class_name "clear-completed";
              on Event.click (fun _ -> Signal.update Todos.clear todos);
              (let@ todos and@ remaining in
               let len = Todos.length todos in
               if len > 0 && len - remaining > 0 then Attr.nop
               else style_list [ ("display", "none") ]);
            ]
            [ text "Clear completed" ];
        ];
    ]

let () =
  match Stdweb.Dom.Document.get_element_by_id "root" with
  | Some root -> Html.mount root (main ())
  | None -> failwith "no #app"
