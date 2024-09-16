module Event = Stdweb.Dom.Event
module Console = Stdweb.Console

let ( => ) a b = (a, b)

open Stdweb
open Helix

let view_function_docs ~signature ~description ~example ?preview ?console func_title =
  let open Html in
  div []
    [
      h2 [ style_list [ ("font-family", "monospace") ] ] [ text func_title ];
      pre [] [ code [ class_name "language-ocaml" ] [ text signature ] ];
      p [] description;
      h3 [] [ text "Example" ];
      pre [] [ code [ class_name "language-ocaml" ] [ text example ] ];
      (match preview with
      | None -> null
      | Some preview ->
        fragment
          [
            h3 [] [ text "Preview" ];
            div [ style_list [ ("padding", "0.5em"); ("background-color", "#F0F0F0") ] ] preview;
          ]);
      (match console with
      | None -> null
      | Some console ->
        fragment
          [
            h3 [] [ text "Console" ]; div [] [ pre [] [ code [ class_name "plaintext" ] console ] ];
          ]);
    ]

module Doc = struct
  let show_values () =
    let open Html in
    div []
      [
        h2 [] [ text "Show values" ];
        text {|Reactive values can be dynamically rendered into the DOM tree.|};
      ]

  let toggle_attributes () =
    let is_visible = Signal.make false in
    let open Html in
    view_function_docs "Toggle attributes" ~signature:"val toggle : on:bool signal -> attr -> attr"
      ~description:
        [
          text "Toggle an attribute based on a boolean signal. If the signal's value is ";
          code [] [ text "true" ];
          text " the attribute is added, otherwise it is omitted";
        ]
      ~example:
        {|let is_visible = Signal.make false in
div []
  [ button
      [ on Event.click (fun _ -> Signal.update not is_visible) ]
      [ text "Toggle" ]
  ; div
      [ toggle ~on:is_visible (style_list [("color", "red")]) ]
      [ text "HELLO" ]
  ]|}
      ~preview:
        [
          button [ on Event.click (fun _ -> Signal.update not is_visible) ] [ text "Toggle" ];
          div [ toggle ~on:Fun.id is_visible (style_list [ ("color", "red") ]) ] [ text "HELLO" ];
        ]
end

let app () =
  let open Html in
  div
    [ style_list [ "display" => "flex" ]; class_list [ "w-full" ] ]
    [
      ul
        [
          style_list
            [
              "position" => "sticky";
              "top" => "0";
              "height" => "100%";
              "width" => "200px";
              "border" => "1px solid red";
            ];
        ]
        [
          li [] [ a [ href "#show_values" ] [ text "Show values" ] ];
          li [] [ a [ href "#toggle_attributes" ] [ text "Toggle attributes" ] ];
        ];
      div [] [ Doc.show_values (); hr []; Doc.toggle_attributes (); hr [] ];
    ]

let () =
  match Dom.Document.get_element_by_id "root" with
  | Some app_el -> Html.mount app_el (app ())
  | None -> failwith "no #app"
