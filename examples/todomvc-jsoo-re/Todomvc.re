open Helix;
module Dom = Stdweb.Dom;

module Jsx = {
  let fragment = arr => Html.fragment(Array.to_list(arr));
  let null = Html.empty;
  let _wrap = (el, attr, children) =>
    el(Array.to_list(attr), Array.to_list(children));
  let _wrap0 = (el, attr) => el(Array.to_list(attr));
  module Elem = {
    let div = _wrap(Html.div);
    let section = _wrap(Html.section);
    let header = _wrap(Html.header);
    let h1 = _wrap(Html.h1);
    let p = _wrap(Html.p);
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
  };
  module Syntax = {
    let option1 = (~attr, opt) =>
      switch (opt) {
      | Some(x) => attr(x)
      | None => Html.Attr.empty
      };

    let option2 = (~attr, v1, v2) => View.toggle(~on=v1, attr(v2));
  };
  let text = Html.text;
};

let main = () => {
  let flag = Signal.make(false);
  let class_list = ["todoapp"];
  let class_list_header = ["header"];
  <section class_list>
    <header class_list=class_list_header>
      <h1> {Html.text("todos")} </h1>
      <input
        autofocus=true
        class_name="new-todo"
        placeholder="What is to be done?"
      />
    </header>
    <button
      on_click={_ => Signal.update((!), flag)}
      style=[("padding", "2em"), ("border", "1px solid grey")]>
      {Html.text("click")}
    </button>
    <div style=?(flag, [("background", "red")])>
      {Html.text("hello")}
    </div>
  </section>;
};

let () =
  switch (Dom.Document.get_element_by_id("root")) {
  | Some(root) => Html.render(root, main())
  | None => failwith("no #app")
  };
