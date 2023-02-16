module Event = Stdweb.Dom.Event
open Helix
open Stdweb
open Signal_syntax

module Todos = struct
  type t =
    { items : (string * bool) list
    ; filter : [ `all | `completed | `remaining ]
    }

  let empty = { items = []; filter = `all }
  let add todo todos = { todos with items = todo :: todos.items }
  let length todos = List.length todos.items

  let remove title todos =
    { todos with items = List.remove_assq title todos.items }

  let toggle target todos =
    { todos with
      items =
        List.map
          (fun (title, completed) ->
            if String.equal title target then (title, not completed)
            else (title, completed))
          todos.items
    }

  let clear todos =
    { todos with
      items = List.filter (fun (_, completed) -> not completed) todos.items
    }

  let count_remaining todos =
    List.fold_left
      (fun n (_, completed) -> if completed then n else n + 1)
      0 todos.items

  let set_filter filter todos = { todos with filter }

  let filtered { items; filter } =
    match filter with
    | `all -> items
    | `completed -> List.filter (fun (_, completed) -> completed) items
    | `remaining -> List.filter (fun (_, completed) -> not completed) items
end

let main () =
  let todos = Signal.make Todos.empty in
  let remaining = Signal.map Todos.count_remaining todos in

  let on_todo_input ev =
    let key = Event.Keyboard.key ev in
    let target = Event.target ev in
    let title = Event.target_value ev in
    if String.equal "Enter" key && String.length title > 0 then (
      Signal.update (Todos.add (title, false)) todos;
      Event.Target.set_value target "")
  in

  let open Html in
  section
    [ class_list [ "todoapp" ] ]
    [ header
        [ class_list [ "header" ] ]
        [ h1 [] [ text "todos" ]
        ; input
            [ class_name "new-todo"
            ; autofocus
            ; placeholder "What is to be done?"
            ; on_keydown on_todo_input
            ]
        ]
    ; section
        [ View.conditional
            ~on:(Signal.map (fun todos -> Todos.length todos > 0) todos)
        ; class_list [ "main" ]
        ]
        [ input
            [ id "toggle-all"; type' "checkbox"; class_list [ "toggle-all" ] ]
        ; label [ for' "toggle-all" ] [ text "Toggle all" ]
        ; ul
            [ class_name "todo-list" ]
            [ todos
              |> Signal.map Todos.filtered
              |> View.each (fun (title, completed) ->
                     li
                       [ class_name "todo" ]
                       [ div
                           [ class_name "view" ]
                           [ input
                               [ class_name "toggle"
                               ; type' "checkbox"
                               ; Attr.on completed checked
                               ; on_click (fun _ ->
                                     Signal.update (Todos.toggle title) todos)
                               ]
                           ; label [] [ text title ]
                           ; button
                               [ class_name "destroy"
                               ; on_click (fun _ ->
                                     Signal.update (Todos.remove title) todos)
                               ]
                               []
                           ]
                       ])
            ]
        ]
    ; footer
        [ class_name "footer" ]
        [ span
            [ class_name "todo-count" ]
            [ strong []
                [ View.show
                    (fun n ->
                      text
                        (string_of_int n
                        ^ (if n = 1 then " item" else " items")
                        ^ " left"))
                    remaining
                ]
            ]
        ; ul
            [ class_name "filters" ]
            [ li []
                [ a
                    [ on_click (fun _ ->
                          Signal.update (Todos.set_filter `all) todos)
                    ]
                    [ text "All" ]
                ]
            ; li []
                [ a
                    [ on_click (fun _ ->
                          Signal.update (Todos.set_filter `remaining) todos)
                    ]
                    [ text "Remaining" ]
                ]
            ; li []
                [ a
                    [ on_click (fun _ ->
                          Signal.update (Todos.set_filter `completed) todos)
                    ]
                    [ text "Completed" ]
                ]
            ]
        ; button
            [ View.conditional
                ~on:
                  (let+ todos and+ remaining in
                   let len = Todos.length todos in
                   len > 0 && len - remaining > 0)
            ; class_name "clear-completed"
            ; on_click (fun _ -> Signal.update Todos.clear todos)
            ]
            [ text "Clear completed" ]
        ]
    ]

let () =
  match Dom.Document.get_element_by_id "root" with
  | Some root -> Html.render root (main ())
  | None -> failwith "no #app"
