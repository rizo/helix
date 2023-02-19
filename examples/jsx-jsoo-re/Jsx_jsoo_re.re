open Helix;
module Dom = Stdweb.Dom;

module Jsx = {
  let null = Html.empty;
  let text = Html.text;
  let fragment = arr => Html.fragment(Array.to_list(arr));

  let _wrap = (el, attr, children) =>
    el(Array.to_list(attr), Array.to_list(children));
  let _wrap0 = (el, attr, _children) => el(Array.to_list(attr));
  module Elem = {
    let div = _wrap(Html.div);
    let section = _wrap(Html.section);
    let header = _wrap(Html.header);
    let h1 = _wrap(Html.h1);
    let ul = _wrap(Html.ul);
    let li = _wrap(Html.li);
    let h2 = _wrap(Html.h2);
    let input = _wrap0(Html.input);
    let button = _wrap(Html.button);
  };
  module Attr = {
    let class_list = v => Html.class_list(v);
    let class_name = v => Html.class_name(v);
    let placeholder = v => Html.placeholder(v);
    let style = v => Html.style(v);
    let on_click = Html.on_click;
    let autofocus = v => if (v) {Html.autofocus} else {Html.Attr.empty};
    let option1 = (attr, opt) =>
      switch (opt) {
      | Some(x) => attr(x)
      | None => Html.Attr.empty
      };

    let option2 = (attr, signal, v) => View.toggle(~on=signal, attr(v));
  };
};

module Show = {
  let make = (~signal, render) => {
    View.show(render, signal);
  };
};

module Each = {
  let make = (~signal, render) => {
    View.each(render, signal);
  };
};

let main = () => {
  let flag = Signal.make(false);
  let items = Signal.make(List.init(7, string_of_int));
  let msg = Signal.map(flag => flag ? "YES" : "NO", flag);
  let class_list = ["todoapp"];
  let class_list_header = ["header"];
  <section class_list>
    <header class_list=class_list_header>
      <h1> "todos" </h1>
      <input
        autofocus=true
        class_name="new-todo"
        placeholder="What is to be done?"
      />
    </header>
    <button
      on_click={_ => Signal.update((!), flag)}
      style=[("padding", "2em"), ("border", "1px solid grey")]>
      "click"
    </button>
    <div
      style=[("outline", "1px solid blue")]
      style=?(flag, [("background", "red")])>
      <text> "hello" </text>
    </div>
    <h2> "Attr.option1" </h2>
    <div style=?None> "optional style: None" </div>
    <div style=?{Some([("color", "red")])}> "optional style: Some" </div>
    <h2> "Fragment" </h2>
    <> <div> "fragment item 1" </div> <div> "fragment item 2" </div> </>
    <h2> "<Show>" </h2>
    <Show signal=msg> {msg => <text> msg </text>} </Show>
    <h2> "<Each>" </h2>
    <button
      on_click={_ => {
        let list = List.init(1 + Random.int(10), string_of_int);
        Signal.emit(list, items);
      }}>
      "Click!"
    </button>
    <ul>
      <Each signal=items> {item => <li> <text> item </text> </li>} </Each>
    </ul>
    <h2> "Spread" </h2>
    <button
      on_click={_ => {
        let list = List.init(1 + Random.int(10), string_of_int);
        Signal.emit(list, items);
      }}>
      "Click!"
    </button>
    {let items = [|
       <div> "spread item 1" </div>,
       <div> "spread item 2" </div>,
     |];
     <div> ...items </div>}
  </section>;
};

let () =
  switch (Dom.Document.get_element_by_id("root")) {
  | Some(root) => Html.render(root, main())
  | None => failwith("no #app")
  };
