# Helix JSX PPX

## Simple elements

Node without any attributes or children:
```reason
<div />;
// Jsx.Elem.div [||] [||]
```

Node with a single child:
```reason
<div> v </div>;
// Jsx.Elem.div [||] [| v |]
```

Node with multiple children:
```reason
<div> v1 v2 </div>;
// Jsx.Elem.div [||] [| v1; v2 |]
```

Node with a literal value child:
```reason
<div> 42 </div>;
// Jsx.Elem.div [||] [| Jsx.int 42 |]
```

Text node:
```reason
<text> "hello" </text>;
<text> v </text>;
// Jsx.Elem.div [||] [| Jsx.text "hello" |]
// Jsx.Elem.div [||] [| Jsx.text v |]
```


## Element attributes

Node with attributes:
```reason
<div a1 a2=2> x </div>;
// Jsx.Elem.div [| Jsx.Attr.a1 a1; Jsx.Attr.a2 2 |] [| x |]
```

Node with optional attributes:
```reason
<div ?a />;
<div a=?x />;
// Jsx.Elem.div [| Jsx.Attr.option1 Jsx.Attr.a a |] [||]
// Jsx.Elem.div [| Jsx.Attr.option1 Jsx.Attr.a x |] [||]

<div a=?(s, x) />;
<div a=?(s, x, y) />;
// Jsx.Elem.div [| Jsx.Attr.option2 Jsx.Attr.a s x |] [||]
// Jsx.Elem.div [| Jsx.Attr.option3 Jsx.Attr.a s x y |] [||]
```


## Fragments

Empty fragment node:
```reason
<> </>;
// Jsx.fragment [||]
```

Fragment with multiple children:
```reason
<> v1 v2 </>;
// Jsx.fragment [| v1; v2 |]
```


## Children spread

Children spread from array:
```reason
<div> ...arr </div>;
<div> ...[|<int>1</int>, <text>"hello"</text>|] </div>;
// Jsx.Elem.div [||] arr
// Jsx.Elem.div [||] [| Jsx.int 1; Jsx.text "hello" |]
```


## Module elements

Module node without any attributes or children:
```reason
<X />;
// X.make ()
```

Module node with attributes and children:
```reason
<X x y=1> v </X>;
// X.make ~x ~y:1 v
```

Module node from a custom function:
```reason
<X.foo x y=1 />;
<X.foo x y=1> v1 v2 </X.foo>;
// X.foo ~x ~y:1 ()
// X.foo ~x ~y:1 [| v1; v2 |]
```