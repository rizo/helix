# Helix

**Build reactive web interfaces in OCaml.**

> Note: this project is experimental. The core functionality is stable but the
> API may break before the official release.

[**API Docs**](https://rizo.github.io/helix/helix/Helix/index.html) • [**Examples**](https://github.com/rizo/helix/tree/master/examples) • [**Starter Project**](https://github.com/rizo/helix-starter)

## Features

- Reactive signals with
  [`signal`](https://github.com/rizo/signal): signals represent values that change over time and can be used to model any dynamic state in your application.
- Declarative HTML with [`Helix.Html`](https://rizo.github.io/helix/html/Html/index.html): write your HTML templates directly in OCaml.
- Fine-grained reactivity without Virtual DOM using
  [show/bind](https://rizo.github.io/helix/helix/Helix/index.html#reactive-views): updates are directly applied to the DOM tree based on values emited by reactive signals.
- Js-compatibility library [`jx`](https://rizo.github.io/helix/jx/Jx/index.html): write bindings to interact withe the JavaScript ecosystem.


## Example

```ocaml
open Helix

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
            [ on_click (fun () -> Signal.update (fun n -> n + 1) count) ]
            [ text "+" ];
          button
            [ on_click (fun () -> Signal.update (fun n -> n - 1) count) ]
            [ text "-" ];
          button [ on_click (fun () -> Signal.emit 0 count) ] [ text "Reset" ];
          div
            [
              style_list [ ("font-size", "32px") ];
              bind
                (fun n -> style_list [ ("color", if n < 0 then "red" else "blue") ])
                count;
            ]
            [ show (fun n -> text (string_of_int n)) count ];
        ];
    ]

let () =
  match Stdweb.Dom.Document.get_element_by_id "root" with
  | Some root -> Html.mount root (counter ())
  | None -> failwith "No #root element found"
```


## Acknowledgements

This library is based on ideas found in other libraries and projects such as:
[Elm](https://elm-lang.org/), [Ur/Web](http://www.impredicative.com/ur/),
[SolidJS](https://www.solidjs.com/),
[petite-vue](https://github.com/vuejs/petite-vue),
[Surplus](https://github.com/adamhaile/surplus),
[Brr](https://erratique.ch/software/brr) and [ReactJS](https://reactjs.org/).


## Troubleshooting

### Build fails with "no cmx file was found"

```
Warning 58 [no-cmx-file]: no cmx file was found in path for module Helix, and its interf
    ace was not compiled with -opaque
```

Helix is built to be used with js_of_ocaml, which itself works with OCaml's bytecode. The above warning suggests that Helix does not provide a native "cmx" file. This is expected since Helix is built with dune's `byte` mode. To resolve this issue, build your executables with `(modes js)` and your libraries with `(modes byte)`. See https://dune.readthedocs.io/en/stable/reference/dune/executable.html#linking-modes.
