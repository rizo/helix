(** Syntax for working with signal values.

    {[
      open Signal.Syntax

      let sum_signal s1 s2 =
        let+ x1 = s1 and+ x2 = s2 in
        x1 + x2
    ]} *)

val ( let+ ) : 'a Signal.t -> ('a -> 'b) -> 'b Signal.t
val ( and+ ) : 'a Signal.t -> 'b Signal.t -> ('a * 'b) Signal.t
val ( <~ ) : ('a -> 'b) -> 'a Signal.t -> 'b Signal.t
val ( ~~ ) : ('a -> 'b) Signal.t -> 'a Signal.t -> 'b Signal.t
