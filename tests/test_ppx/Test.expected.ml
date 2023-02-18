let bt_1 = Jsx.Elem.t [||]
let bt_2 = Jsx.Elem.t [||] [| x |]
let bt_3 = Jsx.Elem.t [||] [| x1; x2 |]
let bt_4 = Jsx.Elem.t1 [||] [| Jsx.Elem.t2 [||] [| x |] |]
let bt_5 = Jsx.Elem.t1 [||] [| Jsx.Elem.t2 [||] [| x |]; Jsx.Elem.t3 [||] |]

let bt_6 =
  Jsx.Elem.t1 [||]
    [| Jsx.Elem.t2 [||] [| x1 |]; Jsx.Elem.t3 [||] [| x2; Jsx.Elem.t4 [||] |] |]

let ba_1 = Jsx.Elem.t [| Jsx.Attr.a 1 |]
let ba_2 = Jsx.Elem.t [| Jsx.Attr.a 1 |] [| x |]
let ba_3 = Jsx.Elem.t [| Jsx.Attr.a1 1; Jsx.Attr.a2 2 |] [| x |]
let ba_4 = Jsx.Elem.t [| Jsx.Attr.a a |]
let ba_5 = Jsx.Elem.t [| Jsx.Attr.a1 a1; Jsx.Attr.a2 2 |] [| x |]
let oa_1 = Jsx.Elem.t [| Jsx.Syntax.option1 ~attr:Jsx.Attr.a a |]
let oa_2 = Jsx.Elem.t [| Jsx.Syntax.option1 ~attr:Jsx.Attr.a x |]
let o2_1 = Jsx.Elem.t [| Jsx.Syntax.option2 ~attr:Jsx.Attr.a s x |]
let o3_1 = Jsx.Elem.t [| Jsx.Syntax.option3 ~attr:Jsx.Attr.a s x y |]

let mx_1 =
  Jsx.Elem.t
    [| Jsx.Attr.a1 1
     ; Jsx.Attr.a2 a2
     ; Jsx.Syntax.option1 ~attr:Jsx.Attr.a3 x3
     ; Jsx.Syntax.option1 ~attr:Jsx.Attr.a4 a4
     ; Jsx.Syntax.option2 ~attr:Jsx.Attr.a5 s 5
     ; Jsx.Syntax.option3 ~attr:Jsx.Attr.a6 s 6 7
    |]
    [| 0 |]
