module Window = Stdweb.Dom.Window

let tick ~ms =
  let s = Signal.make () in
  Window.set_interval (fun () -> Signal.emit () s) ms;
  s
