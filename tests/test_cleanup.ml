module Ev = Stdweb.Dom.Event
module Node = Stdweb.Dom.Node
open Helix

let test_leaky_1 () =
  let n_sig = Signal.make 0 in
  let open Html in
  div
    [ id "root" ]
    [
      show ~label:"baseline_1"
        (fun n -> text ("baseline: " ^ string_of_int n))
        n_sig;
      br [];
      button
        [
          id "leaky_1";
          on Ev.click (fun ev ->
              let node = Ev.target ev in
              match Node.parent node with
              | None -> Jx.log "No parent"
              | Some parent -> Node.remove_child ~parent node
          );
        ]
        [
          show ~label:"leaky_1"
            (fun n -> text ("target: " ^ string_of_int n ^ " - Click to remove"))
            n_sig;
        ];
      br [];
      button
        [ on_click (fun () -> Signal.update (( + ) 1) n_sig) ]
        [ text "Increment" ];
    ]

let test_leaky_2 () =
  let n_sig = Signal.make 0 in
  let open Html in
  let target_1 =
    div
      [ id "leaky_2"; style "border: 1px solid cyan" ]
      [
        show ~label:"leaky_2"
          (fun n -> text ("target_1: " ^ string_of_int n))
          n_sig;
      ]
  in
  div
    [ id "root" ]
    [
      show ~label:"baseline_2"
        (fun n -> text ("baseline: " ^ string_of_int n))
        n_sig;
      br [];
      target_1;
      button
        [ on_click (fun () -> Html.Elem.unmount target_1) ]
        [ text "Delete" ];
      br [];
      button
        [ on_click (fun () -> Signal.update (( + ) 1) n_sig) ]
        [ text "Increment" ];
    ]

let main () =
  let open Html in
  div []
    [
      (* h2 [] [ text "test_leaky_1" ]; *)
      (* test_leaky_1 (); *)
      h2 [] [ text "test_leaky_2" ];
      test_leaky_2 ();
    ]

let () =
  match Stdweb.Dom.Document.get_element_by_id "root" with
  | Some root -> Html.mount root (main ())
  | None -> failwith "no #app"
