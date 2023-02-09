module Window = Stdweb.Dom.Window

let tick ~ms =
  Signal.emitter ~init:() (fun emit ->
      Window.set_interval (fun () -> emit ()) ms)
