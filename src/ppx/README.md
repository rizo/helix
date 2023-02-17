# Helix JSX PPX

Attribute with value:
```ocaml
<div id="active"></div>

Jsx.Elem.div [Jsx.Attr.id "active"] []
```


Attribute with optional value:
```
<div id=?id></div>

Jsx.Elem.div [Jsx.Syntax.option1 ~attr:Jsx.Attr.id id] []
```


Conditional element:
```
<show on=signal><div></div></show>

Jsx.View.show ~on:signal (Jsx.Elem.div [] [])
```


Conditional attribute:
```
<div style=?(signal, my_style)></div>

Jsx.Elem.div [Jsx.Syntax.option2 ~attr:Jsx.Attr.style signal my_style] []
```


Conditional attribute with default:
```
<div style=?(signal, true_style, false_style)></div>

Jsx.Elem.div [Jsx.Syntax.option3 ~attr:Jsx.Attr.style signal true_style false_style] []
```

Reactive list:
```reason
<ul> <each items>{item => <li></li>}</each> </ul>

<ul> <each items>render_item</each> </ul>

Jsx.Elem.ul [] [
  Jsx.View.each (fun item -> Jsx.Elem.li [] []) items
]
```

Event handlers:
```reason
<button on=(Event.click, callback)>"Click!"</button>

Jsx.Elem.button [on Event.click callback] [Jsx.text "Click!"]
```


Render signal value to html:
```
<dyn>{(item_sig as item) => <p>{Html.text(item)}</p>}</dyn>

Jsx.View.dyn (fun item -> Jsx.Elem.p [] [Html.text item]) item_sig
```

```
<dyn>{device => render_device(device)}</dyn>

Jsx.View.dyn render_device device
```

```
<dyn>{(device, status) => render_device_with_status(device, status)}</dyn>

Jsx.View.dyn2 (fun device status -> render_device_with_status device status) device status
```


```
<dyn device>{render_device}</dyn>

Jsx.View.dyn render_device device
```

```
<dyn device status>{render_device_with_status}</dyn>

Jsx.View.dyn2 render_device_with_status device status
```

```
<show device status>{render_device_with_status}</show>
<show on=is_message_visible message>{text}</show>

Jsx.View.dyn2 render_device_with_status device status
```

```reason
/* HTML from value */
<p html=int>x</p>;
```
