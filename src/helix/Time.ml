module Window = Stdweb.Dom.Window

let tick ~ms =
  let s = Signal.make ~equal:(fun _ _ -> false) () in
  Window.set_interval (fun () -> Signal.emit () s) ms;
  s
