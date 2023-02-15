# Helix

**Build reactive web interfaces in OCaml.**

> Note: this project is experimental. The core functionality is stable but the
> API may break before the official release.

[**API Docs**](https://odis-labs.github.io/helix/helix/Helix/index.html) • [**Examples**](https://github.com/odis-labs/helix/tree/master/examples)

## Features

- Reactive signals with
  [`Helix.Signal`](https://odis-labs.github.io/helix/helix/Helix/Signal/index.html):
signals represent values that change over time and can be used to model any
dynamic state in your application.
- Declarative HTML with [`Helix.Html`](https://odis-labs.github.io/helix/helix/Helix/Html/index.html): write your HTML templates directly in OCaml.
- Fine-grained reactivity without Virtual DOM using
  [`Helix.View`](https://odis-labs.github.io/helix/helix/Helix/View/index.html):
updates are directly applied to the real DOM nodes and attributes based on
values emited by reactive signals.
- Js-compatibility library
  [`Helix.Js`](https://odis-labs.github.io/helix/helix/Helix/Js/index.html):
write bindings to interact withe the JavaScript ecosystem.
- Supported compilation backends:
    - [Js_of_ocaml](https://ocsigen.org/js_of_ocaml/latest/manual/overview)
    - [Melange](https://github.com/melange-re/melange)
    - [ReScript](https://rescript-lang.org/)
- Supported compilation frontends:
    - [OCaml](https://ocaml.org)
    - [Reason](https://reasonml.github.io)
    - [ReScript](https://rescript-lang.org)


## Example

```ocaml
open Helix
open Stdweb

let counter () =
  let incr = Signal.make 0 in
  let count = Signal.reduce (fun total n -> total + n) 0 incr in
  let open Html in
  fragment
    [
      h2 [ style [ ("font-family", "monospace") ] ] [ text "Counter" ];
      div [ style [ ("margin-bottom", "20px") ] ] [ text "Compute a count." ];
      div []
        [
          button [ on_click (fun _ -> Signal.emit 1 incr) ] [ text "+" ];
          button [ on_click (fun _ -> Signal.emit (-1) incr) ] [ text "-" ];
          span [ style [ ("margin-left", "5px") ] ] [ View.show int count ];
        ];
    ]

let () =
  match Dom.Document.get_element_by_id "root" with
  | Some root -> Html.render root (counter ())
  | None -> failwith "No #root element found"
```


## Roadmap

- Server-side rendering.
- Declarative JS binding generation.
- Support for scoped CSS styling using web-components.
- JSX ppx for Reason.


## Acknowledgements

This library is based on ideas found in other libraries and projects such as:
[Elm](https://elm-lang.org/), [SolidJS](https://www.solidjs.com/),
[petite-vue](https://github.com/vuejs/petite-vue),
[Surplus](https://github.com/adamhaile/surplus) libraries,
[Brr](https://erratique.ch/software/brr) and [ReactJS](https://reactjs.org/).
