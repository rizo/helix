module Event = Stdweb.Dom.Event
module Console = Stdweb.Console
open Stdweb
open Helix

let view_function_docs ~signature ~description ~example ?preview ?console
    func_title =
  let open Html in
  fragment
    [
      h2 [ style [ ("font-family", "monospace") ] ] [ text func_title ];
      pre [] [ code [ class_name "language-ocaml" ] signature ];
      p [] description;
      h3 [] [ text "Example" ];
      pre [] [ code [ class_name "language-ocaml" ] example ];
      ( match preview with
      | None -> empty
      | Some preview ->
        fragment
          [
            h3 [] [ text "Preview" ];
            div
              [
                style [ ("padding", "0.5em"); ("background-color", "#F0F0F0") ];
              ]
              preview;
          ]
      );
      ( match console with
      | None -> empty
      | Some console ->
        fragment
          [
            h3 [] [ text "Console" ];
            div [] [ pre [] [ code [ class_name "plaintext" ] console ] ];
          ]
      );
    ]

let show_show_docs () =
  let is_visible = Signal.make false in
  let open Html in
  view_function_docs "View.show"
    ~signature:
      "val toggle : on:bool signal -> ('a -> html) -> 'a signal -> html"
    ~description:
      [
        text
          "[show ?on:condition to_html signal] is a reactive HTML element \
           created from\n\
          \    [signal] values using [to_html]. If boolean [condition] signal \
           is passed the\n\
          \    resulting element is only rendered if the signal is [true].";
        code [] "true";
        text " the attribute is added, otherwise it is omitted";
      ]
    ~example:
      {|let is_visible = Signal.make false in
div []
  [ button
      [ on Event.click (fun _ -> Signal.update not is_visible) ]
      [ text "Toggle" ]
  ; div
      [ View.toggle ~on:is_visible (style [("color", "red")]) ]
      [ text "HELLO" ]
  ]|}
    ~preview:
      [
        button
          [ on Event.click (fun _ -> Signal.update not is_visible) ]
          [ text "Toggle" ];
        div
          [ View.toggle ~on:is_visible (style [ ("color", "red") ]) ]
          [ text "HELLO" ];
      ]

let show_html_attr_elem_docs () =
  let open Html in
  view_function_docs "Html.Attr.elem"
    ~signature:"val elem : (html -> unit) -> attr"
    ~description:
      [
        text
          "When this attribute is added, call a function with the attribute's \
           element.";
      ]
    ~example:
      {|button
  [ Attr.elem (fun el -> Console.log ("Button element:", el)) ]
  [ text "Button" ]|}
    ~preview:
      [
        button
          [ Attr.on_mount (fun el -> Console.log ("Button element:", el)) ]
          [ text "Button" ];
      ]
    ~console:{|Array ["Button element", button]|}

let show_toggle_docs () =
  let is_visible = Signal.make false in
  let open Html in
  view_function_docs "View.toggle"
    ~signature:"val toggle : on:bool signal -> attr -> attr"
    ~description:
      [
        text
          "Toggle an attribute based on a boolean signal. If the signal's \
           value is ";
        code [] "true";
        text " the attribute is added, otherwise it is omitted";
      ]
    ~example:
      {|let is_visible = Signal.make false in
div []
  [ button
      [ on Event.click (fun _ -> Signal.update not is_visible) ]
      [ text "Toggle" ]
  ; div
      [ View.toggle ~on:is_visible (style [("color", "red")]) ]
      [ text "HELLO" ]
  ]|}
    ~preview:
      [
        button
          [ on Event.click (fun _ -> Signal.update not is_visible) ]
          [ text "Toggle" ];
        div
          [ View.toggle ~on:is_visible (style [ ("color", "red") ]) ]
          [ text "HELLO" ];
      ]

let app () =
  let selected_function = Signal.make "show" in
  let open Html in
  div
    [ class_list [ "w-full" ] ]
    [
      select
        [
          on Event.change (fun ev ->
              Signal.emit (Event.target_value ev) selected_function
          );
        ]
        [
          option [ value ""; disabled; selected ] [ text "Select function..." ];
          option [ value "show" ] [ text "View.show" ];
          option [ value "toggle" ] [ text "View.toggle" ];
          option [ value "html_attr_elem" ] [ text "Html.Attr.elem" ];
        ];
      hr [];
      selected_function
      |> View.show (fun name ->
             match name with
             (* | "" -> text "Hello!" *)
             | "show" -> show_show_docs ()
             | "toggle" -> show_toggle_docs ()
             | "html_attr_elem" -> show_html_attr_elem_docs ()
             | _ -> text "unknown function name"
         );
    ]

let () =
  match Dom.Document.get_element_by_id "root" with
  | Some app_el -> Html.render app_el (app ())
  | None -> failwith "no #app"
