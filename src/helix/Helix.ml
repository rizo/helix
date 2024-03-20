type html = Html.html
type attr = Html.attr
type 'a signal = 'a Signal.t

let signal = Signal.make

module Html = Html
module Time = Time
module Mouse = Mouse
module History = History
module Http = Http
module Router = Router
include View

let ( let$ ) s f = show f s
let ( and$ ) = Signal.pair
let ( let@ ) s f = bind f s
let ( and@ ) = Signal.pair
