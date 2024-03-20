# Helix

**Build reactive web interfaces in OCaml.**

> Note: this project is experimental. The core functionality is stable but the
> API may break before the official release.

[**API Docs**](https://rizo.github.io/helix/helix/Helix/index.html) • [**Examples**](https://github.com/rizo/helix/tree/master/examples)

## Features

- Reactive signals with
  [`signal`](https://github.com/rizo/signal): signals represent values that change over time and can be used to model any dynamic state in your application.
- Declarative HTML with [`Helix.Html`](https://rizo.github.io/helix/helix/Helix/Html/index.html): write your HTML templates directly in OCaml.
- Fine-grained reactivity without Virtual DOM using
  [`Helix.View`](https://rizo.github.io/helix/helix/Helix/View/index.html): updates are directly applied to the DOM tree based on values emited by reactive signals.
- Js-compatibility library [`jx`](https://github.com/rizo/jx): write bindings to interact withe the JavaScript ecosystem.


## Example

```ocaml
open Helix
open Stdweb

let counter () =
  let count = Signal.make 0 in
  let open Html in
  div
    [ style_list [ ("border", "1px solid #eee"); ("padding", "1em") ] ]
    [
      h2 [] [ text "Counter" ];
      div [] [ text "Compute a count." ];
      div []
        [
          button
            [ on_click (fun _ -> Signal.update (fun n -> n + 1) count) ]
            [ text "+" ];
          button
            [ on_click (fun _ -> Signal.update (fun n -> n - 1) count) ]
            [ text "-" ];
          div
            [
              style_list [ ("font-size", "32px") ];
              bind
                (fun n ->
                  if n < 0 then style_list [ ("color", "red") ]
                  else style_list [ ("color", "blue") ])
                count;
            ]
            [ show (fun n -> text (string_of_int n)) count ];
        ];
    ]

let () =
  match Dom.Document.get_element_by_id "root" with
  | Some root -> Html.mount root (counter ())
  | None -> failwith "No #root element found"
```


## Roadmap

- Add support for [Melange](https://github.com/melange-re/melange).
  - Currently blocked by https://github.com/ocaml/dune/issues/7104.
- Implement a JSX PPX for [Reason](https://reasonml.github.io).
  - WIP implementation: https://github.com/rizo/helix/tree/master/experiments/helix-ppx.
- Server-side rendering.
- Support for scoped CSS styling using web-components.


## Acknowledgements

This library is based on ideas found in other libraries and projects such as:
[Elm](https://elm-lang.org/), [Ur/Web](http://www.impredicative.com/ur/),
[SolidJS](https://www.solidjs.com/),
[petite-vue](https://github.com/vuejs/petite-vue),
[Surplus](https://github.com/adamhaile/surplus),
[Brr](https://erratique.ch/software/brr) and [ReactJS](https://reactjs.org/).
