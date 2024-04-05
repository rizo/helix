module Node = Stdweb.Dom.Node

type t = Jx.t

let t = Jx.global "uPlot"

let data_to_js =
  let open Jx.Encoder in
  array (array float)

let make ~options ~data target =
  let new3 = Jx.Obj.new3 t Jx.Encoder.js data_to_js Node.to_js in
  new3 options data target

let set_data uplot data = Jx.Obj.call1_unit uplot "setData" data_to_js data

let set_size ~w ~h uplot =
  Jx.Obj.call1_unit uplot "setSize" Jx.Encoder.obj
    [ ("width", Jx.Encoder.int w); ("height", Jx.Encoder.int h) ]

let mount ~options ~data uplot_ref =
  Html.Attr.on_mount (fun el -> uplot_ref := Some (make ~options ~data el))
