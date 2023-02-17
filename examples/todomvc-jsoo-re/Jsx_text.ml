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
    let e _a = ()
    let n1 _a _c = ()
    let m _a _c = ()
    let x _a _c = ()
    let l1 _a _c = ()
    let t _a _c = ()
    let o2 _a = ()
  end

  module Syntax = struct
    let option1 ~attr:_ _1 _2 = ()
  end
end

module X = struct
  let make _attrs = ()
end

module Y = struct
  let make _attrs _children = ()
end

(* <e />;
   <n1> <m>1</m> </n1>;
   <x a=5>"hey"</x>;
   <l1>a b c</l1>;
   <t a x=1 ?o m=?mi>5</t>;
   <X a=5 />;
   <Y a=5>1 2</Y>;
   <>5</>;

   <o2 x=?(s, c) />;
*)
let a = 101
let o = 102
let mi = 103
let _ = e ~children:[] () [@JSX]
let _ = n1 ~children:[ (m ~children:[ 1 ] () [@JSX]) ] () [@JSX]
let _ = x ~a:5 ~children:[ "hey" ] () [@JSX]
let _ = l1 ~children:[ 1; 2; 3 ] () [@JSX]
let _ = t ~a ~x:1 ?o ?m:mi ~children:[ 5 ] () [@JSX]
let _ = X.createElement ~a:5 ~children:[] () [@JSX]
let _ = Y.createElement ~a:5 ~children:[ 1; 2 ] () [@JSX]
let _ = [ 5 ] [@JSX]

(* Reactive syntax *)
let _ = o2 ?x:(1, 2) ~children:[] () [@JSX]
