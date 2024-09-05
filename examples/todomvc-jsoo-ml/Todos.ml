type t = { items : (string * bool) list; filter : [ `all | `completed | `remaining ] }

let empty = { items = []; filter = `all }
let add todo todos = { todos with items = todo :: todos.items }
let length todos = List.length todos.items
let remove title todos = { todos with items = List.remove_assq title todos.items }

let toggle target todos =
  {
    todos with
    items =
      List.map
        (fun (title, completed) ->
          if String.equal title target then (title, not completed) else (title, completed)
        )
        todos.items;
  }

let clear todos =
  { todos with items = List.filter (fun (_, completed) -> not completed) todos.items }

let count_remaining todos =
  List.fold_left (fun n (_, completed) -> if completed then n else n + 1) 0 todos.items

let set_filter filter todos = { todos with filter }

let filtered { items; filter } =
  match filter with
  | `all -> items
  | `completed -> List.filter (fun (_, completed) -> completed) items
  | `remaining -> List.filter (fun (_, completed) -> not completed) items
