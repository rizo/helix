let column ?distribute ?gap ?(align_x = `left) ?(align_y = `top)
    ?(stretch = false) ?(reverse = false) () =
  let out = [ "w-full"; "h-auto"; "box-border"; "flex"; "flex-no-wrap" ] in
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
    | Some `xxxs -> "gap-y-xxxs" :: out
    | Some `xxs -> "gap-y-xxs" :: out
    | Some `xs -> "gap-y-xs" :: out
    | Some `sm -> "gap-y-sm" :: out
    | Some `md -> "gap-y-md" :: out
    | Some `lg -> "gap-y-lg" :: out
    | Some `xl -> "gap-y-xl" :: out
    | Some `xxl -> "gap-y-xxl" :: out
    | Some `xxxl -> "gap-y-xxxl" :: out
  in
  let out =
    match align_x with
    | `left -> "items-start" :: out
    | `center -> "items-center" :: out
    | `right -> "items-end" :: out
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

let row ?distribute ?gap ?(align_x = `left) ?(align_y = `top) ?(stretch = false)
    ?(reverse = false) () =
  let out =
    [ "w-full"; "h-auto"; "box-border"; "inline-flex"; "flex-no-wrap" ]
  in
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
    | Some `xxxs -> "gap-x-xxxs" :: out
    | Some `xxs -> "gap-x-xxs" :: out
    | Some `xs -> "gap-x-xs" :: out
    | Some `sm -> "gap-x-sm" :: out
    | Some `md -> "gap-x-md" :: out
    | Some `lg -> "gap-x-lg" :: out
    | Some `xl -> "gap-x-xl" :: out
    | Some `xxl -> "gap-x-xxl" :: out
    | Some `xxxl -> "gap-x-xxxl" :: out
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
