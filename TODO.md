# TODO

- [x] Consider forcing each element to be a unique instance: `html = unit -> node`.
- [ ] Rename Html.fragment to list or inline. Fragments are not real document fragments.
- [ ] Signal.not.
- [ ] Test toggle + conditional attr.
- [ ] Add Js.String module.
- [ ] Repeated attribute.
- [ ] View.toggle : on:('a signal -> bool) -> 'a signal -> attr -> attr
- [ ] Html.Attr.join
- [ ] Html: append/insert have different insertion orders...
- [ ] Signal.pair sync/transaction - prevent updates when both are firing sequentially. In the router this currently triggers a double work if both wars are needed as in /a/:int/:string for fetching. We need to sync/step emitting to the route var/args signals and emits should trigger only one pub to subs.
- [ ] Add ?equal to Helix.show.
- [ ] Rationale for using ppx: value restriction for route/path combinators.
- [ ] When rendering links, should each link have vars as signals or is better to render N links at once from a single show?
- [ ] Consider representing the path vars with Hmap to allow any view to obtain a typed signal var.
- [ ] Do signals need to be resource aware and clean up subs on exn?
- [ ] `Signal.sync s1 s2` to fully sync two signals?
- [ ] When emitting to a routing var signal should we navigate?
- [x] Is it worth adding `let$`? `let ( let$ ) s f = Helix.show (fun x -> f x) s`.
- [ ] Should there be a way to create lazy signals? E.g. Mouse.position should only bind mouse events when someone is subbed.
- [ ] Add prevent default handling to Html.on.
- [ ] Check if Router.link ~absolute:true sets the active attr with a relative router.
- [ ] Var from routes with spaces are not %-decoded when emited to signals.
- [ ] Consider passing `path` instead of `rotuer`. This makes it easier to manage relative links.
    - Define base path ops for router paths (with prefix/rest).
- [ ] Consider making Router.link relative by default to avoid having to pass router for absolute routes. Instead expose ~relative:router.
- [ ] Repurpose Jx to a JS FFI module.
- [ ] Http.get ~url
- [ ] BUG? Sharing html references leads to problems (See shared_ref)
- [ ] Consider structuring each router segment as "path?opts" to allow passing extra params to intermediate views.

## shared_ref

```
    

let format_options_scalar_html =
  List.map
    (fun v -> Html.option [ Html.value v ] [ Html.text v ])
    [ "Int"; "Float"; "String"; "Binary"; "Enum" ]


let render_format (format : Api_types.Format.t) =
  let open Html in
  let selected_sig = Signal.make format in
  Ui.column ~gap:`sm div []
    [
      Ui.row ~gap:`sm div []
        [
          Ui.flex (Ui.label "Type") []
            [
              Ui.select
                [ (*value (Dev_attr_fmt.name format);
                    on Event.change (fun ev ->
                        let value = Node.get_value (Event.target ev) in
                        let new_format = default_format_of_string value |> or_invalid_arg value in
                        Signal.emit new_format selected_sig
                    );*) ]
                format_options_all_html;
            ];
    ...
```

This leads to:

```
Fatal error: exception Failure("bug: attempting to remove a text node without a parent: Int") 
```
