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

// Inferred type
let it_1 = <t> "hello" </t>;
let it_2 = <t> 5 </t>;
let it_3 = <t> 3.14 </t>;

// Typed tags
let tt_1 = <text> "hello" </text>;
let tt_2 = <int> 42 </int>;
let tt_3 = <float> 3.14 </float>;
let tt_4 = <t> <int> 100 </int> </t>;
