type 'a signal = 'a Signal.t
type html = Html.html
type attr = Html.attr
type js = Helix_js.js

module Js = Helix_js
module Html = Html
module Signal = Signal
module View = View
module Time = Time
module Mouse = Mouse

let render = Html.render
