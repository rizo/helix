module Element = Stdweb.Dom.Element
module Js = Helix.Js

type t = Js.t

let t = Js.global "uPlot"

let data_to_js =
  let open Js.Encoder in
  array (array float)

let make ~options ~data target =
  let new3 = Js.Obj.new3 t Js.Encoder.js data_to_js Element.to_js in
  new3 options data target

let set_data uplot data = Js.Obj.call1_unit uplot "setData" data_to_js data

let mount ~options ~data uplot_ref =
  Helix.Html.Attr.on_mount (fun el ->
      uplot_ref := Some (make ~options ~data el))
