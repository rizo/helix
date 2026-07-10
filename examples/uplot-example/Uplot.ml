module Node = Stdweb.Dom.Node
module E = Jx.Encode

type t = Jx.t

let t = Jx.global "uPlot"
let data_to_js data = data |> Array.map (fun row -> row |> Array.map E.float |> E.array) |> E.array
let make ~options ~data target = Jx.obj_new t [| options; data_to_js data; Node.to_js target |]
let set_data uplot data = ignore (Jx.obj_call uplot "setData" [| data_to_js data |])

let set_size ~w ~h uplot =
  ignore (Jx.obj_call uplot "setSize" [| E.obj [| ("width", E.int w); ("height", E.int h) |] |])

let mount ~options ~data uplot_ref =
  Html.Attr.on_mount (fun node -> uplot_ref := Some (make ~options ~data node))
