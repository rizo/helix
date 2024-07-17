(* The first draft implementation of Helix componenets. *)

module Common = struct
  let h_sm = "h-7"
  let h_md = "h-8"
  let h_lg = "h-9"
  let w_sm = "w-7"
  let w_md = "w-8"
  let w_lg = "w-9"
  let px_sm = "px-2.5"
  let px_md = "px-3"
  let px_lg = "px-3"
  let text_sm = "text-xs"
  let text_md = "text-sm"
  let text_lg = "text-normal"
  let text_xl = "text-lg"
  let text_xl2 = "text-xl"
  let text_xl3 = "text-2xl"
  let text_xl4 = "text-3xl"
end

module Col = struct
  let data = Html.Attr.bool "data-helix-column" true

  let style ?(full = true) ?distribute ?gap ?align_x ?(align_y = `top) ?(stretch = false)
      ?(reverse = false) () =
    let out = [ "h-auto"; "box-border"; "flex"; "flex-no-wrap" ] in
    let out = if full then "w-full" :: out else out in
    let out =
      match distribute with
      | None -> out
      | Some `between -> "justify-between" :: out
      | Some `around -> "justify-around" :: out
      | Some `evenly -> "justify-evenly" :: out
    in
    let out =
      match gap with
      | None -> out
      | Some `none -> "gap-y-none" :: out
      | Some `xxxs -> "gap-y-1/2" :: out
      | Some `xxs -> "gap-y-1" :: out
      | Some `xs -> "gap-y-2" :: out
      | Some `sm -> "gap-y-3" :: out
      | Some `md -> "gap-y-4" :: out
      | Some `lg -> "gap-y-5" :: out
      | Some `xl -> "gap-y-6" :: out
      | Some `xxl -> "gap-y-8" :: out
      | Some `xxxl -> "gap-y-10" :: out
    in
    let out =
      match align_x with
      | Some `left -> "items-start" :: out
      | Some `center -> "items-center" :: out
      | Some `right -> "items-end" :: out
      | None -> out
    in
    let out =
      match align_y with
      | `top -> "justify-start" :: out
      | `center -> "justify-center" :: out
      | `bottom -> "justify-end" :: out
    in
    let out = if stretch then "items-stretch" :: out else out in
    let out = if reverse then "flex-col-reverse" :: out else "flex-col" :: out in
    out
end

let col ?full ?distribute ?gap ?align_x ?align_y ?stretch ?reverse elem attrs children =
  elem
    (Col.data
    :: Html.class_list (Col.style ?full ?distribute ?gap ?align_x ?align_y ?stretch ?reverse ())
    :: attrs
    )
    children

module Row = struct
  let style ?(full = true) ?distribute ?gap ?(align_x = `left) ?(align_y = `top) ?(stretch = false)
      ?(reverse = false) () =
    let out = [ "box-border"; "inline-flex"; "flex-no-wrap" ] in
    let out = if full then "w-full" :: out else out in
    let out =
      match distribute with
      | None -> out
      | Some `between -> "justify-between" :: out
      | Some `around -> "justify-around" :: out
      | Some `evenly -> "justify-evenly" :: out
    in
    let out =
      match gap with
      | None -> out
      | Some `none -> "gap-x-none" :: out
      | Some `xxxs -> "gap-x-1/2" :: out
      | Some `xxs -> "gap-x-1" :: out
      | Some `xs -> "gap-x-2" :: out
      | Some `sm -> "gap-x-3" :: out
      | Some `md -> "gap-x-4" :: out
      | Some `lg -> "gap-x-5" :: out
      | Some `xl -> "gap-x-6" :: out
      | Some `xxl -> "gap-x-8" :: out
      | Some `xxxl -> "gap-x-10" :: out
    in
    let out =
      match align_x with
      | `left -> "justify-start" :: out
      | `center -> "justify-center" :: out
      | `right -> "justify-end" :: out
    in
    let out =
      match align_y with
      | `top -> "items-start" :: out
      | `center -> "items-center" :: out
      | `bottom -> "items-end" :: out
    in
    let out = if stretch then "items-stretch" :: out else out in
    let out = if reverse then "flex-row-reverse" :: out else "flex-row" :: out in
    out
end

let row ?full ?distribute ?gap ?align_x ?align_y ?stretch ?reverse elem attrs children =
  elem
    (Html.class_list (Row.style ?full ?distribute ?gap ?align_x ?align_y ?stretch ?reverse ())
    :: attrs
    )
    children

module Align = struct
  let style ?(full = true) ?(x = `center) ?(y = `center) () =
    let out = [ "flex" ] in
    let out = if full then "w-full" :: out else out in
    let out =
      match x with
      | `left -> "justify-start" :: out
      | `center -> "justify-center" :: out
      | `right -> "justify-end" :: out
    in
    let out =
      match y with
      | `top -> "items-start" :: out
      | `center -> "items-center" :: out
      | `bottom -> "items-end" :: out
    in
    out
end

let align ?x ?y ?full elem attrs children =
  elem (Html.class_list (Align.style ?x ?y ?full ()) :: attrs) children

module Flex = struct
  let style ?(full = true) ?(wrap = true) () =
    let out = [ "flex" ] in
    let out = if full then "w-full" :: out else out in
    let out = if wrap then "flex-wrap" :: out else out in
    out
end

let flex ?full ?wrap elem attrs children =
  elem (Html.class_list (Flex.style ?full ?wrap ()) :: attrs) children

module Button = struct
  let base =
    [
      "shrink-0";
      "disabled:bg-zinc-300";
      "disabled:cursor-not-allowed";
      "disabled:border-0";
      "disabled:text-zinc-500";
    ]

  let blank = [ "bg-white"; "text-zinc-900"; "hover:bg-zinc-50"; "active:bg-zinc-100" ]
  let default = [ "bg-zinc-900"; "text-white"; "hover:bg-zinc-700"; "active:bg-zinc-800" ]
  let primary = [ "bg-blue-600"; "text-white"; "hover:bg-blue-500"; "active:bg-blue-700" ]
  let secondary = [ "bg-zinc-200"; "text-zinc-900"; "hover:bg-zinc-50"; "active:bg-zinc-100" ]
  let danger = [ "bg-red-600"; "text-white"; "hover:bg-red-500"; "active:bg-red-700" ]

  let outline =
    [
      "border";
      "border-zinc-200";
      "bg-white";
      "text-zinc-900";
      "hover:bg-zinc-50";
      "active:bg-zinc-100";
    ]
end

let button ?(full = false) ?(group = false) ?(gap = `xxs) ?(style = `default) ?(elem = Html.button)
    ?(size = `md) ?(square = false) attrs children =
  let cl = Button.base @ Row.style ~full ~gap ~align_y:`center ~align_x:`center () in
  let cl = if group then [ "first:rounded-l"; "last:rounded-r" ] @ cl else [ "rounded" ] @ cl in
  let cl =
    match size with
    | `auto -> cl
    | `sm when square -> Common.[ h_sm; w_sm; text_sm ] @ cl
    | `md when square -> Common.[ h_md; w_md; text_md ] @ cl
    | `lg when square -> Common.[ h_lg; w_lg; text_lg ] @ cl
    | `sm -> Common.[ h_sm; px_sm; text_sm ] @ cl
    | `md -> Common.[ h_md; px_md; text_md ] @ cl
    | `lg -> Common.[ h_lg; px_lg; text_lg ] @ cl
  in
  let cl_attr =
    match style with
    | `blank -> Html.class_list (Button.blank @ cl)
    | `default -> Html.class_list (Button.default @ cl)
    | `primary -> Html.class_list (Button.primary @ cl)
    | `secondary -> Html.class_list (Button.secondary @ cl)
    | `danger -> Html.class_list (Button.danger @ cl)
    | `outline -> Html.class_list (Button.outline @ cl)
    | `none -> Html.class_list cl
  in
  elem (cl_attr :: attrs) children

module Select = struct
  let style =
    [
      "helix-select";
      "flex";
      "px-3";
      "h-8";
      "py-1";
      "bg-gray-100";
      "border-2";
      "border-gray-100";
      "outline-0";
      "text-gray-900";
      "rounded";
      "focus:border-blue-500";
      "block";
      "cursor-pointer";
      "disabled:cursor-not-allowed";
    ]
    @ [ Common.text_md ]

  let overlay =
    [
      "helix-select-null";
      "absolute";
      "inset-0";
      "appearance-none";
      "opacity-0";
      "w-full";
      "focus-within:ring-blue-500";
      "focus-within:border-blue-500";
      "cursor-pointer";
    ]
end

let select ?(full = true) ?empty:(empty_arg = "No values") attrs children =
  let cl = if full then "w-full" :: Select.style else Select.style in
  let attrs = Html.class_list cl :: attrs in
  let attrs = if List.is_empty children then Html.disabled true :: attrs else attrs in
  let children =
    if List.is_empty children then
      [ Html.option [ Html.value ""; Html.disabled true ] [ Html.text empty_arg ] ]
    else children
  in
  Html.select attrs children

let select_custom elem ?(overlay = []) attrs options =
  Html.div
    (Html.class_list [ "relative"; "flex" ] :: attrs)
    [ elem; Html.select (overlay @ [ Html.class_list Select.overlay ]) options ]

module Input = struct
  let data_1p_ignore = Html.attr "data-1p-ignore" ""

  let style =
    [
      "helix-input";
      "flex";
      "px-3";
      "py-1";
      "h-8";
      "bg-gray-100";
      "border-2";
      "border-gray-100";
      "outline-0";
      "text-gray-900";
      "rounded";
      "focus:border-blue-500";
      "block";
    ]
    @ [ Common.text_md ]

  let date =
    [
      "helix-input";
      "pl-3";
      "h-8";
      "bg-gray-100";
      "border-0";
      "outline-0";
      "text-gray-900";
      "rounded";
      "focus:ring-blue-500";
      "focus:border-blue-500";
    ]
    @ [ Common.text_md ]
end

let input ?(full = true) attrs =
  let cl = Input.style in
  let cl = if full then "w-full" :: cl else cl in
  Html.input (Html.class_list cl :: attrs)

let input_date attrs = Html.input (Html.type' "date" :: Html.class_list Input.date :: attrs)
let input_file attrs = Html.input (Html.type' "file" :: Html.class_list Input.style :: attrs)

module Textarea = struct
  let style =
    [
      "helix-input";
      "flex";
      "w-full";
      "min-h-9";
      "px-3";
      "py-1";
      "bg-gray-100";
      "border-2";
      "border-gray-100";
      "outline-0";
      "text-gray-900";
      "rounded";
      "focus:border-blue-500";
    ]
end

let textarea attrs children = Html.textarea (Html.class_list Textarea.style :: attrs) children

let label ?(full = true) text attrs children =
  let cl = [] in
  let cl = if full then "w-full" :: cl else cl in
  Html.label (Html.class_list cl :: attrs)
    (Html.span [ Html.class_list [ "font-semibold" ] ] [ Html.text text ] :: children)

module Icon = struct
  let size = function
    | `xs -> [ "h-4"; "w-4" ]
    | `sm -> [ "h-5"; "w-5" ]
    | `md -> [ "h-6"; "w-6" ]
    | `lg -> [ "h-8"; "w-8" ]
    | `xl -> [ "h-9"; "w-9" ]
    | `xl2 -> [ "h-14"; "w-14" ]
    | `xl3 -> [ "h-16"; "w-16" ]
    | `xl4 -> [ "h-20"; "w-20" ]
    | `xl5 -> [ "h-24"; "w-24" ]
end

let icon ?(size = `md) icon_html attrs =
  let cl = Icon.size size @ [ "flex"; "justify-center" ] in
  Html.Elem.unsafe "span" (Html.class_list cl :: attrs) icon_html

module A = struct
  let style = [ "underline"; "hover:text-blue-600" ]
end

let a attrs children = Html.a (Html.class_list A.style :: attrs) children

module H = struct
  let make ?(full = true) ?(bold = true) level =
    let cl = [] in
    let size_c =
      match level with
      | `h1 -> "text-3xl"
      | `h2 -> "text-2xl"
      | `h3 -> "text-lg"
      | `h4 -> "text-md"
    in
    let cl = size_c :: cl in
    let cl = if bold then "font-bold" :: cl else cl in
    let cl = if full then "w-full" :: cl else cl in
    cl
end

let h1 ?full ?bold attrs children =
  Html.h1 (Html.class_list (H.make ?full ?bold `h1) :: attrs) children

let h2 ?full ?bold attrs children =
  Html.h2 (Html.class_list (H.make ?full ?bold `h2) :: attrs) children

let h3 ?full ?bold attrs children =
  Html.h3 (Html.class_list (H.make ?full ?bold `h3) :: attrs) children

let h4 ?full ?bold attrs children =
  Html.h4 (Html.class_list (H.make ?full ?bold `h4) :: attrs) children

module Frame = struct
  let data = Html.Attr.bool "data-helix-frame" true

  let cl =
    Html.class_list
      [ "p-6"; "max-w-6xl"; "mx-auto"; "*:border"; "*:rounded"; "*:bg-white"; "*:px-4"; "*:py-6" ]
end

let frame elem attrs children = elem (Frame.data :: Frame.cl :: attrs) children

module Table = struct
  let data = Html.Attr.bool "data-helix-table" true
  let th_cl = [ "font-semibold"; "text-zinc-500"; "py-1" ]

  let make_align align =
    match align with
    | `left -> [ "text-left" ]
    | `center -> [ "text-center" ]
    | `right -> [ "text-right"; "*:ml-auto" ]

  let view_header columns =
    let open Html in
    thead
      [ class_list [] ]
      [
        tr
          [ class_list [ "text-left"; "bg-white"; "border-b" ] ]
          (List.map
             (fun (title, user_align, _) ->
               th [ class_list th_cl; class_list (make_align user_align) ] [ title ]
             )
             columns
          );
      ]
end

let table ?(align_y = `top) ?(full = true) ?empty:(user_empty = Html.empty) ~columns attrs data =
  let open Html in
  let tr_cl = [ "border-b" ] in
  let tr_cl =
    match align_y with
    | `top -> "align-top" :: tr_cl
    | `center -> "align-middle" :: tr_cl
    | `bottom -> "align-bottom" :: tr_cl
  in
  let cl = [ "overflow-x-scroll" ] in
  let cl = if full then "w-full" :: cl else cl in
  div
    (Table.data :: class_list cl :: attrs)
    [
      table
        [ class_list [ "min-w-max"; "w-full"; "border-collapse" ] ]
        [
          Table.view_header columns;
          tbody
            [ class_list [] ]
            (List.map
               (fun entry ->
                 tr
                   [ class_list tr_cl ]
                   (List.map
                      (fun (_, user_align, get) ->
                        td
                          [ class_list [ "py-1" ]; class_list (Table.make_align user_align) ]
                          [ get entry ]
                      )
                      columns
                   )
               )
               data
            );
        ];
      ( match data with
      | [] -> user_empty
      | _ -> Html.empty
      );
    ]

let record ?(align = `left) ?(full = true) ~schema attrs datum =
  let open Html in
  let cl = [ "flex"; "flex-col" ] in
  let cl = if full then "w-full" :: cl else cl in
  let td_cl = [ "w-full" ] in
  let td_cl =
    match align with
    | `left -> "text-left" :: td_cl
    | `right -> "text-right" :: td_cl
  in
  let td_cl = td_cl @ [] in
  let rows =
    List.map
      (fun (field_title, get_field_value) ->
        tr
          [ class_list [ "flex" ] ]
          [
            th [ class_list (Table.th_cl @ [ "w-full"; "text-left" ]) ] [ field_title ];
            td [ class_list td_cl ] [ get_field_value datum ];
          ]
      )
      schema
  in
  table (class_list cl :: attrs) rows

module Note = struct
  let base =
    [ "py-2"; "px-2"; "rounded-md"; "border" ] @ Row.style ~stretch:true ~align_y:`top ~gap:`xs ()

  let warning = [ "border-yellow-400"; "text-yellow-800" ] @ base
  let error = [ "border-red-400"; "text-red-800"; "bg-red-50" ] @ base
  let info = [ "bg-blue-50"; "border-blue-400"; "text-gray-800" ] @ base
  let warning_icon () = icon ~size:`sm Icons.warning []
  let error_icon () = icon ~size:`sm Icons.error []
  let info_icon () = icon ~size:`sm Icons.info []
end

let note ?(full = true) ?role:(user_role = `info) attrs children =
  let open Html in
  let icon, cl =
    match user_role with
    | `warning -> Note.(warning_icon (), warning)
    | `error -> Note.(error_icon (), error)
    | `info -> Note.(info_icon (), info)
  in
  let cl = if full then "w-full" :: cl else cl in
  let attrs' = class_list cl :: attrs in
  div attrs' [ icon; div [ class_list [ "text-sm" ] ] children ]

module Toggle = struct
  let container_class = "relative inline-flex items-center cursor-pointer"
  let input_class = "sr-only peer"

  let main_class =
    "w-11 h-6 bg-gray-200 peer-focus:outline-none peer-focus:ring-1 peer-focus:ring-blue-300 \
     rounded-full peer peer-checked:after:translate-x-full \
     rtl:peer-checked:after:-translate-x-full peer-checked:after:border-white after:content-[''] \
     after:absolute after:top-[2px] after:start-[2px] after:bg-white after:border-gray-300 \
     after:border after:rounded-full after:h-5 after:w-5 after:transition-all \
     peer-checked:bg-blue-600"

  let label_class = "ms-3 text-sm font-medium text-gray-900"
end

let toggle ?checked:(user_checked = false) ?value:(user_value = "") attrs =
  let open Html in
  label
    (class_name Toggle.container_class :: attrs)
    [
      input
        [ type' "checkbox"; checked user_checked; value user_value; class_name Toggle.input_class ];
      div (class_name Toggle.main_class :: attrs) [];
    ]

module Spinner = struct end

let spinner attrs =
  let open Html in
  Elem.unsafe "div" (attr "role" "status" :: attrs)
    {|<svg aria-hidden="true" class="w-8 h-8 text-gray-200 animate-spin fill-blue-600" viewBox="0 0 100 101" fill="none" xmlns="http://www.w3.org/2000/svg">
        <path d="M100 50.5908C100 78.2051 77.6142 100.591 50 100.591C22.3858 100.591 0 78.2051 0 50.5908C0 22.9766 22.3858 0.59082 50 0.59082C77.6142 0.59082 100 22.9766 100 50.5908ZM9.08144 50.5908C9.08144 73.1895 27.4013 91.5094 50 91.5094C72.5987 91.5094 90.9186 73.1895 90.9186 50.5908C90.9186 27.9921 72.5987 9.67226 50 9.67226C27.4013 9.67226 9.08144 27.9921 9.08144 50.5908Z" fill="currentColor"/>
        <path d="M93.9676 39.0409C96.393 38.4038 97.8624 35.9116 97.0079 33.5539C95.2932 28.8227 92.871 24.3692 89.8167 20.348C85.8452 15.1192 80.8826 10.7238 75.2124 7.41289C69.5422 4.10194 63.2754 1.94025 56.7698 1.05124C51.7666 0.367541 46.6976 0.446843 41.7345 1.27873C39.2613 1.69328 37.813 4.19778 38.4501 6.62326C39.0873 9.04874 41.5694 10.4717 44.0505 10.1071C47.8511 9.54855 51.7191 9.52689 55.5402 10.0491C60.8642 10.7766 65.9928 12.5457 70.6331 15.2552C75.2735 17.9648 79.3347 21.5619 82.5849 25.841C84.9175 28.9121 86.7997 32.2913 88.1811 35.8758C89.083 38.2158 91.5421 39.6781 93.9676 39.0409Z" fill="currentFill"/>
    </svg>
    <span class="sr-only">Loading...</span>|}

let pulse attrs =
  let open Html in
  span
    (class_name "relative flex h-3 w-3" :: attrs)
    [
      span
        [
          class_name
            "animate-ping absolute inline-flex h-full w-full rounded-full bg-blue-400 opacity-75";
        ]
        [];
      span [ class_name "relative inline-flex rounded-full h-3 w-3 bg-blue-500" ] [];
    ]

module Tabs = struct
  let active = Html.class_list [ "border-b-4"; "border-gray-900"; "text-zinc-900" ]
end

let tabs ?(aside = Html.empty) attrs items =
  row Html.div
    (Html.class_list [ "border-b"; "border-gray-200" ] :: attrs)
    (List.map
       (fun (title, link) ->
         Html.span
           [ Html.class_list [ "flex"; "-mb-px" ] ]
           [
             Html.a
               [ link ~active:Tabs.active; Html.class_list [ "me-6"; "py-2"; "text-zinc-600" ] ]
               [ title ];
           ]
       )
       items
    @ [ aside ]
    )

module Placeholder = struct
  let container_cl = [ "py-12"; "border" ]
  let title_cl = [ "font-normal"; "text-lg"; "text-gray-600" ]
end

let placeholder ?title:user_title ?(icon = Html.empty) ?(message = Html.empty) attrs children =
  col ~align_x:`center ~gap:`md Html.div
    (Html.class_list Placeholder.container_cl :: attrs)
    ([
       icon;
       begin
         match user_title with
         | None -> Html.empty
         | Some user_title ->
           Html.span [ Html.class_list Placeholder.title_cl ] [ Html.text user_title ]
       end;
       Html.span [ Html.class_list [ "font-light"; "text-gray-400"; "text-md" ] ] [ message ];
     ]
    @ children
    )

let dropdown button_children content_children =
  let open Html in
  div
    [ class_list [ "relative"; "inline-block"; "text-left"; "dropdown" ] ]
    [
      label
        [ class_list [ "outline"; "outline-red-600"; "cursor-pointer" ] ]
        [
          (* div [] [ button [ type' "button" ] button_children ]; *)
          span [] [ text "Drop!" ];
          input [ class_name "dropdown_checkbox"; type' "checkbox"; checked false ];
          div
            [
              class_list
                [
                  "dropdown_content";
                  "absolute";
                  "right-0";
                  "z-10";
                  "mt-2";
                  "w-56";
                  "origin-top-right";
                  "divide-y";
                  "divide-gray-100";
                  "rounded-md";
                  "bg-white";
                  "shadow-lg";
                  "ring-1";
                  "ring-black";
                  "ring-opacity-5";
                  "focus:outline-none";
                ];
              role "menu";
              attr "aria-orientation" "vertical";
              attr "aria-labelledby" "menu-button";
              tabindex (-1);
            ]
            [ div [ class_name "px-4 py-3"; role "none" ] content_children ];
        ];
    ]

(* Form *)
let form ?(full = true) ?(default = false) ~submit attrs_arg children =
  let module Event = Stdweb.Dom.Event in
  let module Form_data = Stdweb.Form_data in
  let on_submit_attr =
    Html.on ~default Event.submit (fun ev -> ev |> Event.target |> Form_data.make |> submit)
  in
  let cl = [] in
  let cl = if full then "w-full" :: cl else cl in
  let attrs = [ Html.class_list cl; on_submit_attr ] @ attrs_arg in
  Html.form attrs children

let badge ?(size = `sm) ?(style = `info) attrs user_text =
  let cl = [ "px-2"; "py-1"; "text-xs"; "rounded-full" ] in
  let cl =
    match style with
    | `info -> [ "bg-blue-50"; "text-indigo-900" ] @ cl
  in
  let open Html in
  span (class_list cl :: attrs) [ text user_text ]

module Text = struct
  let size = function
    | `sm -> Common.text_sm
    | `md -> Common.text_md
    | `lg -> Common.text_lg
end

let code ?(size = `md) attr string =
  Html.code (Html.class_list [ Text.size size; "text-gray-700" ] :: attr) string

let code_block attrs ?header:header_arg data =
  let open Html in
  let lines = String.split_on_char '\n' data in
  let children =
    List.mapi
      (fun i l ->
        div
          [ class_list [ "flex" ] ]
          [
            span
              [ class_list [ "select-none"; "text-gray-500"; "min-w-9"; "text-right"; "px-2" ] ]
              [ text (string_of_int i) ];
            text l;
          ]
      )
      lines
  in
  let pre_el =
    pre
      (class_list
         [ "bg-gray-800"; "text-white"; "p-2"; "rounded-b-md"; "overflow-scroll"; "text-sm" ]
      :: attrs
      )
      [ code [] children ]
  in
  match header_arg with
  | None -> pre_el
  | Some header_el ->
    div attrs
      [
        div
          [ class_list [ "bg-gray-600"; "text-white"; "py-2"; "px-4"; "rounded-t-md"; "text-sm" ] ]
          [ header_el ];
        pre_el;
      ]

let text ?(upper = false) ?size ?weight attrs_arg text_arg =
  let cl = [] in
  let cl =
    match size with
    | None -> cl
    | Some `sm -> Common.text_sm :: cl
    | Some `md -> Common.text_md :: cl
    | Some `lg -> Common.text_lg :: cl
    | Some `xl -> Common.text_xl :: cl
    | Some `xl2 -> Common.text_xl2 :: cl
    | Some `xl3 -> Common.text_xl3 :: cl
    | Some `xl4 -> Common.text_xl4 :: cl
  in
  let cl =
    match weight with
    | None -> cl
    | Some `thin -> "font-thin" :: cl
    | Some `extralight -> "font-extralight" :: cl
    | Some `light -> "font-light" :: cl
    | Some `normal -> "font-normal" :: cl
    | Some `medium -> "font-medium" :: cl
    | Some `semibold -> "font-semibold" :: cl
    | Some `bold -> "font-bold" :: cl
    | Some `extrabold -> "font-extrabold" :: cl
    | Some `black -> "font-black" :: cl
  in
  let cl = if upper then "uppercase" :: cl else cl in
  if List.is_empty cl then
    if List.is_empty attrs_arg then Html.text text_arg
    else Html.span attrs_arg [ Html.text text_arg ]
  else Html.span (Html.class_list cl :: attrs_arg) [ Html.text text_arg ]
