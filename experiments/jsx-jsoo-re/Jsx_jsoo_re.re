open Helix;
module Dom = Stdweb.Dom;

module Jsx = {
  // let null = Html.empty;
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
    let h3 = _wrap(Html.h3);
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

    let option2 = (attr, bool, v) => bool ? attr(v) : Html.Attr.empty;
    // View.toggle(~on=signal, attr(v));
  };
};

module Show = {
  let make = (~signal, render) => {
    View.show(render, signal);
  };

  let text = (~signal, ()) => {
    View.show(Html.text, signal);
  };

  let int = (~signal, ()) => {
    View.show(Html.int, signal);
  };

  let list = (~signal, ()) => View.each(html => html, signal);
};

module Each = {
  let make = (~signal, render) => {
    View.each(render, signal);
  };
};

let main = () => {
  let flag = Signal.make(false);
  let items = Signal.make(List.init(5, string_of_int));
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
      style=?(true, [("background", "red")])>
      <text> "hello" </text>
    </div>
    <h2> "Attr.option1" </h2>
    <div style=?None> "optional style: None" </div>
    <div style=?{Some([("color", "red")])}> "optional style: Some" </div>
    <h2> "Fragment" </h2>
    <> <div> "fragment item 1" </div> <div> "fragment item 2" </div> </>
    <div>
      <h3> "<Show>" </h3>
      <Show signal={Signal.make("Hello")}> {msg => <text> msg </text>} </Show>
      <h3> "<Show.text>" </h3>
      <Show.text signal={Signal.make("Hello")} />
      <h3> "<Show.int>" </h3>
      <Show.int signal={Signal.make(42)} />
      <h3> "<Show.list>" </h3>
      <Show.list
        signal={Signal.make([
          <div> "item 1" </div>,
          <div> "item 2" </div>,
          <div> "item 3" </div>,
        ])}
      />
    </div>
    <div>
      <h3> "Each" </h3>
      <Each signal={Signal.make(["item 1", "item 2", "item 3"])}>
        {item => <div> <text> item </text> </div>}
      </Each>
    </div>
    <div>
      <h2> "Show.list" </h2>
      <button
        on_click={_ => {
          let list = List.init(1 + Random.int(5), string_of_int);
          Signal.emit(list, items);
        }}>
        "Click!"
      </button>
      <ul>
        <Show.list
          signal={Signal.map(
            List.map(item => <li> <text> item </text> </li>),
            items,
          )}
        />
      </ul>
    </div>
    <h2> "Spread" </h2>
    <button
      on_click={_ => {
        let list = List.init(1 + Random.int(5), string_of_int);
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
  | Some(root) => Html.mount(root, main())
  | None => failwith("no #app")
  };
