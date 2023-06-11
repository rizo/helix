module Html = Html
module Signal = Signal
module View = View
module Time = Time
module Mouse = Mouse

type 'a signal = 'a Signal.t
type html = Html.html
type attr = Html.attr

let render = Html.render
