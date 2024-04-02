open Stdweb

let _on_todo_input ev =
  let open Dom in
  let key = Event.key ev in
  let target = Event.target ev in
  let title = Node.get_value (Event.target ev) in
  if String.equal "Enter" key && String.length title > 0 then
    Node.set_value target ""

let () =
  match Dom.Document.query "#main" with
  | Some main ->
    let btn = Dom.Document.create_element "button" in
    Dom.Node.set_text_content btn "Button";

    let inp = Dom.Document.create_element "input" in
    let lnk = Dom.Document.create_element "a" in
    Dom.Node.set_text_content lnk "Link";
    Dom.Node.set_href lnk "#";

    Dom.Node.append_child ~parent:main btn;
    Dom.Node.append_child ~parent:main inp;
    Dom.Node.append_child ~parent:main lnk;

    (* Element listener *)
    Dom.Node.bind btn Dom.Event.click (fun ev ->
        let target = Dom.Event.target ev in
        Console.log target;
        Console.log ("click: " ^ string_of_float (Dom.Event.page_x ev))
    );

    (* HTMLElement listener *)
    Dom.Node.bind inp Dom.Event.change (fun ev ->
        let target = Dom.Event.target ev in
        Console.log
          (Jx.Obj.get_path (Dom.Node.to_js target) [ "constructor" ]
             Jx.Decoder.any
          );
        Console.log target;
        Console.log "change"
    );

    (* Window listener *)
    Dom.Window.bind Dom.Event.resize (fun ev ->
        let target = Dom.Event.target ev in
        Console.log target;
        Console.log "resize"
    )
  | None -> failwith "no #main"
