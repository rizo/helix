# Helix


## Recommendations

### Reusing the same element

When creating elements with the [Html] module it is important to avoid reusig the same element.

Consider the following example:

```ocaml
let html =
  let open Html in
  let message = text "Hello" in
  div [] [
    message;
    message;
  ]
```

This will only render message once and not twice! This is because the element instances in a DOM tree must be unique, therefore the repeated element will be moved from its previous location in DOM to the last used location in DOM.

To avoid this problem, make sure to instantiate new instances of the desired repeated element:

```ocaml
let html =
  let open Html in
  let message () = text "Hello" in
  div [] [
    message ();
    message ();
  ]
```

This works as expected.