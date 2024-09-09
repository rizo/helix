open struct
  module Event = Stdweb.Dom.Event
  module Document = Stdweb.Dom.Document
  module Node = Stdweb.Dom.Node
end

open Helix
open Signal.Syntax

let main () =
  let todos = Signal.make Todos.empty in
  (* let filteredTodos =
       Signal.map2
         (fun todos path ->
           match path with
           | [ "remaining" ] -> Todos.set_filter `remaining todos
           | [ "completed" ] -> Todos.set_filter `completed todos
           | _ -> todos
         )
         todos Helix.History.hash_path
     in *)
  let remaining = Signal.map Todos.count_remaining todos in

  let on_todo_input ev =
    let key = Event.key ev in
    let target = Event.target ev in
    let title = Node.get_value target in

    if String.equal "Enter" key && String.length title > 0 then (
      Signal.update (Todos.add (title, false)) todos;
      Node.set_value target ""
    )
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
      conditional
        ~on:(Signal.map (fun todos -> Todos.length todos > 0) todos)
        (section
           [ class_list [ "main" ] ]
           [
             input [ id "toggle-all"; type' "checkbox"; class_list [ "toggle-all" ] ];
             label [ for' "toggle-all" ] [ text "Toggle all" ];
             ul
               [ class_name "todo-list" ]
               [
                 todos
                 |> Signal.map Todos.filtered
                 |> each (fun (title, completed) ->
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
                                    Attr.on completed (checked true);
                                    on Event.click (fun _ ->
                                        Signal.update (Todos.toggle title) todos
                                    );
                                  ];
                                label [] [ text title ];
                                button
                                  [
                                    class_name "destroy";
                                    on Event.click (fun _ ->
                                        Signal.update (Todos.remove title) todos
                                    );
                                  ]
                                  [];
                              ];
                          ]
                    );
               ];
           ]
        );
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
                   |> text
                  );
                ]
              (* [ show
                     (fun n ->
                       [ string_of_int n; (if n = 1 then "item" else "items"); "left" ]
                       |> String.concat " "
                       |> text
                     )
                     remaining
                 ] *);
            ];
          ul
            [ class_name "filters" ]
            [
              li []
                [
                  a
                    [ on Event.click (fun _ -> Signal.update (Todos.set_filter `all) todos) ]
                    [ text "All" ];
                ];
              li []
                [
                  a
                    [ on Event.click (fun _ -> Signal.update (Todos.set_filter `remaining) todos) ]
                    [ text "Remaining" ];
                ];
              li []
                [
                  a
                    [ on Event.click (fun _ -> Signal.update (Todos.set_filter `completed) todos) ]
                    [ text "Completed" ];
                ];
            ];
          conditional
            ~on:
              (let+ todos and+ remaining in
               let len = Todos.length todos in
               len > 0 && len - remaining > 0
              )
            (button
               [
                 class_name "clear-completed";
                 on Event.click (fun _ -> Signal.update Todos.clear todos);
               ]
               [ text "Clear completed" ]
            );
        ];
    ]

let () =
  match Document.get_element_by_id "root" with
  | Some root -> Html.mount root (main ())
  | None -> failwith "no #app"
