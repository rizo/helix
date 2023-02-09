# Helix JSX PPX

```ocaml
(* Attribute with value. *)
<div id="active"></div>

Jsx.Elem.div [Jsx.Attr.id "active"] []


(* Attribute with optional value. *)
<div id=?id></div>

Jsx.Elem.div [(match id with None -> Jsx.Attr.empty | Some id -> Jsx.Attr.id id)] []


<div class_list=["active"] conditional=is_active>
  "I'm active"
</div>

Jsx.div [
  Jsx.Attr.class_list ["active"];
  Jsx.Attr.conditional is_active
] [
  Jsx.text "I'm active!"
]


(* Functional attributes *)
<div style_list=my_styles>
  5
</div>

Jsx.div [
  Jsx.Attr.style_list (toggle [("background-color", "red")])
] [
  Jsx.text "I'm active!"
]

```

