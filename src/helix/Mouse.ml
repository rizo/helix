module Event = Stdweb.Dom.Event
module Window = Stdweb.Dom.Window

let position_of_mouse_event m_ev = Event.(page_x m_ev, page_y m_ev)

let position =
  Signal.emitter ~init:(0.0, 0.0) (fun emit ->
      Window.bind Event.mousemove (fun m_ev ->
          emit (position_of_mouse_event m_ev)
      )
  )
