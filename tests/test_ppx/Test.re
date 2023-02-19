// All variations
let no_children = <div />;
let single_var_child = <div> v </div>;

let multi_var_children = <div> v1 v2 </div>;

let single_lit_child = <div> 42 </div>;

let mixed_children = <div> v 42 </div>;

let empty_frag = <> </>;
let single_var_frag = <> v </>;
let multi_var_frag = <> v1 v2 </>;
let single_lit_frag = <> 42 </>;
let mixed_frag = <> v1 42 </>;
let lit_node = <int> 42 </int>;
let spread_children = <div> ...arr </div>;
let mod_a0_c0 = <X />;
let mod_a1_c0 = <X x=1 />;
let mod_a2_c0 = <X x y=1 />;
let mod_a2_c1 = <X x y=1> v </X>;

/*
 // Basic tags
 let bt_1 = <t />;
 let bt_2 = <t> x </t>;
 let bt_3 = <t> x1 x2 </t>;
 let bt_4 = <t1> <t2> x </t2> </t1>;
 let bt_5 = <t1> <t2> x </t2> <t3 /> </t1>;
 let bt_6 = <t1> <t2> x1 </t2> <t3> x2 <t4 /> </t3> </t1>;

 // Basic attributes
 let ba_1 = <t a=1 />;
 let ba_2 = <t a=1> x </t>;
 let ba_3 = <t a1=1 a2=2> x </t>;
 let ba_4 = <t a />;
 let ba_5 = <t a1 a2=2> x </t>;

 // Option attributes
 let oa_1 = <t ?a />;
 let oa_2 = <t a=?x />;

 // Option 2-tuple attributes
 let o2_1 = <t a=?(s, x) />;

 // Option 3-tuple attributes
 let o3_1 = <t a=?(s, x, y) />;

 // Mixed attributes
 let mx_1 = <t a1=1 a2 a3=?x3 ?a4 a5=?(s, 5) a6=?(s, 6, 7)> 0 </t>;
 let mx_2 =
   <t1 a1=1 a2 a3=?x3 ?a4 a5=?(s, 5) a6=?(s, 6, 7)> <t2> e </t2> </t1>;

 // Inferred type
 let it_1 = <t> "hello" </t>;
 let it_2 = <t> 5 </t>;
 let it_3 = <t> 3.14 </t>;

 // Typed tags
 let tt_1 = <text> "hello" </text>;
 let tt_2 = <int> 42 </int>;
 let tt_3 = <float> 3.14 </float>;
 let tt_4 = <t> <int> 100 </int> </t>;
 let tt_5 = <text> some_str </text>;
 let tt_6 = <int> some_int </int>;
 let tt_7 = <float> some_float </float>;

 // Children
 let ch_1 = <t />;
 let ch_2 = <t> 1 </t>;
 let ch_3 = <t> e </t>;
 let ch_4 = <t> e1 e1 </t>;
 let ch_5 = <t> <> 1 </> </t>;
 let ch_6 = <t> {[1, 1]} </t>;
 let ch_7 = <t1> <t2 /> <t3 /> </t1>;
 let ch_8 = <t1> <t2 /> e 1 <t3 /> </t1>;
 */
