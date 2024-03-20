module Event = Stdweb.Dom.Event
module Window = Stdweb.Dom.Window

let position_of_mouse_event m_ev = Event.(page_x m_ev, page_y m_ev)

let position () =
  let s = Signal.make (0.0, 0.0) in
  Window.bind Event.mousemove (fun m_ev -> Signal.emit (position_of_mouse_event m_ev) s);
  s
