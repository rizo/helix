module Jsx = struct
  let fragment _children = ()
  let null = ()

  module Attr = struct
    let a _v = ()
    let x _v = ()
    let o _v = ()
    let m _v = ()
  end

  module Elem = struct
    let e _a _c = ()
    let n _a _c = ()
    let m _a _c = ()
    let x _a _c = ()
    let l _a _c = ()
    let t _a _c = ()
    let o _a _c = ()
  end
end

module X = struct
  let make _attrs = ()
end

module Y = struct
  let make _attrs _children = ()
end

(* <e />;
   <n> <m>1</m> </n>;
   <x a=5>"hey"</x>;
   <l>a b c</l>;
   <t a x=1 ?o m=?mi>5</t>;
   <X a=5 />;
   <Y a=5>1 2</Y>;
   <>5</>;

   <t x=?(s, c) />;
*)
let a = 101
let o = 102
let mi = 103
let _ = e ~children:[] () [@JSX]
let _ = n ~children:[ (m ~children:[ 1 ] () [@JSX]) ] () [@JSX]
let _ = x ~a:5 ~children:[ "hey" ] () [@JSX]
let _ = l ~children:[ 1; 2; 3 ] () [@JSX]
let _ = t ~a ~x:1 ?o ?m:mi ~children:[ 5 ] () [@JSX]
let _ = X.createElement ~a:5 ~children:[] () [@JSX]
let _ = Y.createElement ~a:5 ~children:[ 1; 2 ] () [@JSX]
let _ = [ 5 ] [@JSX]

(* Reactive syntax *)
let _ = o ?x:(1, 2) ~children:[] () [@JSX]
