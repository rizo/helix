type 'a signal = 'a Signal.t
type html = Html.html
type attr = Html.attr

module Signal = Signal
module Html = Html
module View = View
module Time = Time
module Mouse = Mouse

let render = Html.render
