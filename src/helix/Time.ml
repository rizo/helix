module Window = Stdweb.Dom.Window

let tick ~ms =
  let s = Signal.make ~equal:(fun _ _ -> false) () in
  let _ = Window.set_interval (fun () -> Signal.emit () s) ms in
  s
