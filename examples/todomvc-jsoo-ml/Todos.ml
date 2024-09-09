module Todos = Map.Make (Int)

let gen_id =
  let i = ref (-1) in
  fun () ->
    incr i;
    !i

type item = { title : string; completed : bool }
type t = { items : item Todos.t; filter : [ `all | `completed | `remaining ] }

let empty = { items = Todos.empty; filter = `all }

let add title todos =
  { todos with items = Todos.add (gen_id ()) { title; completed = false } todos.items }

let length todos = Todos.cardinal todos.items
let remove id todos = { todos with items = Todos.remove id todos.items }

let toggle id todos =
  {
    todos with
    items =
      Todos.update id
        (function
          | None -> None
          | Some item -> Some { item with completed = not item.completed }
          )
        todos.items;
  }

let clear todos =
  { todos with items = Todos.filter (fun _ { completed; _ } -> not completed) todos.items }

let count_remaining todos =
  Todos.fold (fun _id { completed; _ } n -> if completed then n else n + 1) todos.items 0

let filter filter todos = { todos with filter }

let filtered { items; filter } =
  match filter with
  | `all -> Todos.to_list items
  | `completed -> Todos.to_list (Todos.filter (fun _id { completed; _ } -> completed) items)
  | `remaining -> Todos.to_list (Todos.filter (fun _id { completed; _ } -> not completed) items)
