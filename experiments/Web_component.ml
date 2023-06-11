open Stdweb

(*

   class PopUpInfo extends HTMLElement {
     constructor() {
       super();
     }
   }

   class PopUpInfo extends HTMLElement {
     constructor() {
       // Always call super first in constructor
       super();

       // Create a shadow root
       const shadow = this.attachShadow({ mode: "open" });

       // Create spans
       const wrapper = document.createElement("span");
       wrapper.setAttribute("class", "wrapper");

       const icon = document.createElement("span");
       icon.setAttribute("class", "icon");
       icon.setAttribute("tabindex", 0);

       const info = document.createElement("span");
       info.setAttribute("class", "info");

       // Take attribute content and put it inside the info span
       const text = this.getAttribute("data-text");
       info.textContent = text;

       // Insert icon
       const img = document.createElement("img");
       img.src = this.hasAttribute("img")
         ? this.getAttribute("img")
         : "img/default.png";
       icon.appendChild(img);

       // Create some CSS to apply to the shadow dom
       const style = document.createElement("style");
       console.log(style.isConnected);

       style.textContent = `
         .wrapper {
           position: relative;
         }
         .info {
           font-size: 0.8rem;
           width: 200px;
           display: inline-block;
           border: 1px solid black;
           padding: 10px;
           background: white;
           border-radius: 10px;
           opacity: 0;
           transition: 0.6s all;
           position: absolute;
           bottom: 20px;
           left: 10px;
           z-index: 3;
         }
         img {
           width: 1.2rem;
         }
         .icon:hover + .info, .icon:focus + .info {
           opacity: 1;
         }
       `;

       // Attach the created elements to the shadow dom
       shadow.appendChild(style);
       console.log(style.isConnected);
       shadow.appendChild(wrapper);
       wrapper.appendChild(icon);
       wrapper.appendChild(info);
     }
   }
*)

module Class = struct
  let extend ~constr parent =
    let parent_proto = Js.Obj.create (Js.Obj.get_prototype parent) in
    Js.Obj.set_prototype constr parent_proto;
    Js.Obj.set_path constr [ "prototype"; "constructor" ] Js.Encoder.js constr
end

let main () =
  let child_constructor () =
    Js.log "child_constructor"
    (* Js.Obj.call1_unit Dom.Html_element.t "call" Js.Encoder.any child_constructor *)
  in
  let child_constructor_js = Js.Encoder.fun0 child_constructor in
  Class.extend ~constr:child_constructor_js Dom.Html_element.t;
  Dom.Custom_element_registry.define "my-element" child_constructor_js;
  Js.log (Js.Obj.new0 child_constructor_js)
