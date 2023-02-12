(* https://github.com/leeoniya/uPlot/blob/42938f988a9611d0a65f98a52f018d2edd3b8076/demos/tooltips.html *)

open Stdweb

let js_of_data data =
  Metajs.js_of_array (Metajs.js_of_array Metajs.js_of_float) data

let uPlot = Metajs.obj_get Metajs.global "uPlot"

let init ~options data target =
  let data = js_of_data data in
  let target = Dom.Element.as_js target in
  let _ = Metajs.obj_new uPlot [| options; data; target |] in
  ()

let options =
  let open Metajs in
  obj
    [|
      ("title", js_of_string "Tooltips");
      ("width", js_of_int 600);
      ("height", js_of_int 400);
      ("scales", obj [| ("x", obj [| ("time", js_of_bool false) |]) |]);
      ( "series",
        js_of_array obj
          [|
            [||];
            [| ("label", js_of_string "One"); ("stroke", js_of_string "red") |];
            [|
              ("label", js_of_string "Two");
              ("stroke", js_of_string "blue");
              ("show", js_of_bool false);
            |];
          |] );
    |]

let data =
  [|
    [| 1.; 2.; 3.; 4.; 5.; 6.; 7. |];
    [| 40.; 43.; 60.; 65.; 71.; 73.; 80. |];
    [| 18.; 24.; 37.; 55.; 55.; 60.; 63. |];
  |]

let main () =
  let open Helix.Html in
  div [ Attr.on_mount (fun el -> init ~options data el) ] []

let () =
  match Dom.Document.get_element_by_id "root" with
  | Some root -> Helix.render root (main ())
  | None -> failwith "no #root found"
