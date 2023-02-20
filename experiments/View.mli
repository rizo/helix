(* val show : ?on:bool Signal.t -> ('a -> Html.html) -> 'a Signal.t -> Html.html *)
(* [show ?on:condition to_html signal] is a reactive HTML element created
    from [signal] values using [to_html]. If boolean [condition] signal is
    passed the resulting element is only rendered if the signal is [true]. *)
(* val filter : ('a -> bool) -> 'a Signal.t -> Html.html -> Html.html *)
(* val optional : ('a -> Html.html option) -> 'a Signal.t -> Html.html *)
(* val conditional : on:bool Signal.t -> 'a Signal.t -> Html.html *)
(* val on : bool Signal.t -> Html.html -> Html.html *)
(* val option : ('a -> Html.html) -> 'a option Signal.t -> Html.html *)
(* [option to_html items] reactively renders [items] with [to_html].

    {[
      let name = Signal.make None in
      div [] [ optional string name ]
    ]} *)
