let ( let+ ) s f = Signal.map f s
let ( and+ ) = Signal.pair
let ( <~ ) = Signal.map
let ( ~~ ) = Signal.apply
