module Signal = Helix.Signal
module Js = Helix.Js

open struct
  let ensure msg b = if not b then Js.log ("ensure: " ^ msg)

  let expect msg e a =
    if e <> !a then
      let e_js = Stdweb.Array.of_list e in
      let a_js = Stdweb.Array.of_list !a in
      let out =
        Js.Encoder.(pair string Stdweb.Dict.to_js)
          (msg, Stdweb.Dict.of_array [| ("expected", e_js); ("actual", a_js) |])
      in
      Js.log out

  let reject msg = Js.log ("rejected: " ^ msg)
  let push out x = out := List.append !out [ x ]
end

(* get *)

let test_get_1 () =
  let o = ref [] in
  let s = Signal.make 1 in
  push o (Signal.get s);
  push o (Signal.get s);
  Signal.emit 2 s;
  push o (Signal.get s);
  expect "get_1" [ 1; 1; 2 ] o

let test_get_2 () =
  let o = ref [] in
  let s1 = Signal.make 1 in
  push o (`s1 (Signal.get s1));
  let s2 = Signal.map (( + ) 100) s1 in
  push o (`s2 (Signal.get s2));
  Signal.emit 2 s1;
  push o (`s1 (Signal.get s1));
  push o (`s2 (Signal.get s2));
  expect "get_2" [ `s1 1; `s2 101; `s1 2; `s2 102 ] o

(* use *)

let test_use_1 () =
  let o = ref [] in
  let s = Signal.make 1 in
  Signal.use (push o) s;
  expect "use_1" [ 1 ] o

let test_use_2 () =
  let o = ref [] in
  let s = Signal.make 1 in
  Signal.use (push o) s;
  Signal.use (push o) s;
  expect "use_2" [ 1; 1 ] o

(* sub *)

let test_sub_1 () =
  let o = ref [] in
  let s = Signal.make 1 in
  Signal.sub (push o) s;
  expect "sub_1" [] o

let test_sub_2 () =
  let o = ref [] in
  let s = Signal.make 1 in
  Signal.sub (push o) s;
  Signal.emit 2 s;
  expect "sub_1" [ 2 ] o

(* emit *)

let test_emit_1 () =
  let o = ref [] in
  let s = Signal.make 1 in
  Signal.use (push o) s;
  Signal.emit 2 s;
  expect "set_1" [ 1; 2 ] o

let test_emit_2 () =
  let o = ref [] in
  let s = Signal.make 1 in
  Signal.use (push o) s;
  Signal.use (push o) s;
  Signal.emit 2 s;
  expect "set_2" [ 1; 1; 2; 2 ] o

let test_emit_3 () =
  let o = ref [] in
  let s = Signal.make 1 in
  Signal.use (push o) s;
  Signal.emit 2 s;
  Signal.use (push o) s;
  expect "set_3" [ 1; 2; 2 ] o

(* map *)

let test_map_1 () =
  let o = ref [] in
  let s1 = Signal.make 1 in
  let s2 = Signal.map string_of_int s1 in
  ensure "map_1" (Signal.get s2 = "1");
  Signal.use (fun x -> push o (`s2 x)) s2;
  expect "map_1" [ `s2 "1" ] o

let test_map_2 () =
  let o = ref [] in
  let s1 = Signal.make 1 in
  let s2 = Signal.map string_of_int s1 in
  Signal.use (fun x -> push o (`s2_1 x)) s2;
  Signal.use (fun x -> push o (`s2_2 x)) s2;
  expect "map_2" [ `s2_1 "1"; `s2_2 "1" ] o

let test_map_3 () =
  let o = ref [] in
  let s1 = Signal.make 1 in
  let s2 = Signal.map string_of_int s1 in
  Signal.use (fun x -> push o (`s1 x)) s1;
  Signal.use (fun x -> push o (`s2 x)) s2;
  expect "map_3" [ `s1 1; `s2 "1" ] o

let test_map_4 () =
  let o = ref [] in
  let s1 = Signal.make 1 in
  Signal.use (fun x -> push o (`s1 x)) s1;
  let s2 = Signal.map string_of_int s1 in
  Signal.use (fun x -> push o (`s2 x)) s2;
  Signal.emit 2 s1;
  Signal.emit 3 s1;
  Signal.emit "100" s2;
  expect "map_4" [ `s1 1; `s2 "1"; `s1 2; `s2 "2"; `s1 3; `s2 "3"; `s2 "100" ] o

let test_filter_map_1 () =
  let o = ref [] in
  let s1 = Signal.make 0 in
  let s2 =
    Signal.filter_map (fun x -> if x = 0 then None else Some x) ~seed:100 s1
  in
  Signal.use (fun x -> push o x) s2;
  Signal.emit 2 s1;
  Signal.emit 0 s1;
  Signal.emit 3 s1;
  expect "filter_map_1" [ 100; 2; 3 ] o

(* tap *)

let test_tap_1 () =
  let o = ref [] in
  let s1 = Signal.make 1 in
  let s2 = Signal.tap (fun x -> push o (`s2_tap x)) s1 in
  Signal.use (fun x -> push o (`s2 x)) s2;
  Signal.emit 2 s1;
  expect "tap_1" [ `s2 1; `s2_tap 2; `s2 2 ] o

(* map_emit *)

let test_map_emit_1 () =
  let o = ref [] in
  let s1 = Signal.make 1 in
  let s2 = Signal.map string_of_int s1 in
  Signal.use (push o) s2;
  Signal.use (push o) s2;
  Signal.emit 2 s1;
  expect "map_emit_1" [ "1"; "1"; "2"; "2" ] o

let test_map_emit_2 () =
  let o = ref [] in
  let s = Signal.make 1 in
  Signal.use (fun x -> push o (`i x)) s;
  let s' = Signal.map string_of_int s in
  Signal.use (fun x -> push o (`s x)) s';
  Signal.emit 10 s;
  Signal.emit "100" s';
  expect "map_emit_2" [ `i 1; `s "1"; `i 10; `s "10"; `s "100" ] o

let test_map_emit_3 () =
  let o = ref [] in
  let s = Signal.make 1 in
  Signal.use (fun x -> push o (`i x)) s;
  Signal.use (fun x -> push o (`i x)) s;
  let s' = Signal.map string_of_int s in
  Signal.use (fun x -> push o (`s x)) s';
  Signal.use (fun x -> push o (`s x)) s';
  Signal.emit 10 s;
  Signal.emit "100" s';
  expect "map_emit_3"
    [ `i 1
    ; `i 1
    ; `s "1"
    ; `s "1"
    ; `i 10
    ; `i 10
    ; `s "10"
    ; `s "10"
    ; `s "100"
    ; `s "100"
    ]
    o

let test_map_emit_4 () =
  let o = ref [] in
  let s = Signal.make 1 in
  Signal.use (fun x -> push o (`i x)) s;
  Signal.emit 2 s;
  let s' = Signal.map string_of_int s in
  Signal.use (fun x -> push o (`s x)) s';
  Signal.emit 3 s;
  Signal.emit "4" s';
  expect "map_emit_4" [ `i 1; `i 2; `s "2"; `i 3; `s "3"; `s "4" ] o

(* filter *)

let test_filter_1 () =
  let o = ref [] in
  let s = Signal.make 1 in
  let s' = Signal.filter (fun x -> x mod 2 = 0) ~seed:0 s in
  Signal.use (push o) s';
  expect "filter_1" [ 0 ] o

let test_filter_2 () =
  let o = ref [] in
  let s = Signal.make 2 in
  let s' = Signal.filter (fun x -> x mod 2 = 0) ~seed:0 s in
  Signal.use (push o) s';
  expect "filter_2" [ 2 ] o

let test_filter_3 () =
  let o = ref [] in
  let s1 = Signal.make 1 in
  let s2 = Signal.filter (fun x -> x mod 2 = 0) ~seed:0 s1 in
  Signal.use (push o) s2;
  Signal.emit 2 s1;
  Signal.emit 3 s1;
  Signal.emit 4 s2;
  Signal.emit 5 s2;
  expect "filter_3" [ 0; 2; 4 ] o

let test_reduce_1 () =
  let o = ref [] in
  let s = Signal.make 1 in
  let s' = Signal.reduce ( + ) 0 s in
  Signal.use (push o) s';
  Signal.emit 2 s;
  Signal.emit 3 s;
  expect "reduce_1" [ 1; 3; 6 ] o

let test_reduce_2 () =
  let o = ref [] in
  let s1 = Signal.make 1 in
  Signal.use (fun x -> push o (`s1 x)) s1;
  let s2 = Signal.reduce ( + ) 0 s1 in
  Signal.use (fun x -> push o (`s2 x)) s2;
  Signal.emit 2 s1;
  Signal.emit 3 s1;
  Signal.emit 4 s2;
  Signal.emit 100 s1;
  expect "reduce_2"
    [ `s1 1; `s2 1; `s1 2; `s2 3; `s1 3; `s2 6; `s2 4; `s1 100; `s2 104 ]
    o

(* select *)

let test_select_1 () =
  let o = ref [] in
  let s1 = Signal.make 1 in
  let s2 = Signal.make 100 in
  let s3 = Signal.select [ s1; s2 ] in
  Signal.use (push o) s3;
  expect "select_1" [ 1 ] o

let test_select_2 () =
  let o = ref [] in
  let s1 = Signal.make 10 in
  Signal.use (fun x -> push o (`s1 x)) s1;
  let s2 = Signal.make 20 in
  Signal.use (fun x -> push o (`s2 x)) s2;
  let s3 = Signal.make 30 in
  Signal.use (fun x -> push o (`s3 x)) s3;
  let s4 = Signal.select [ s1; s2; s3 ] in
  Signal.use (fun x -> push o (`s4 x)) s4;
  Signal.emit 11 s1;
  Signal.emit 21 s2;
  Signal.emit 31 s3;
  Signal.emit 40 s4;
  expect "select_2"
    [ `s1 10
    ; `s2 20
    ; `s3 30
    ; `s4 10
    ; `s1 11
    ; `s4 11
    ; `s2 21
    ; `s4 21
    ; `s3 31
    ; `s4 31
    ; `s4 40
    ]
    o

let test_select_3 () =
  try
    let _s = Signal.select [] in
    reject "select_3"
  with Invalid_argument _ -> ()

(* uniq *)

let test_uniq_1 () =
  let o = ref [] in
  let s1 = Signal.make 1 in
  Signal.use (fun x -> push o (`s1 x)) s1;
  let s2 = Signal.uniq s1 in
  Signal.use (fun x -> push o (`s2 x)) s2;
  Signal.emit 1 s2;
  Signal.emit 1 s2;
  Signal.emit 10 s2;
  Signal.emit 10 s1;
  Signal.emit 11 s1;
  Signal.emit 11 s2;
  expect "uniq_1" [ `s1 1; `s2 1; `s2 10; `s1 10; `s1 11; `s2 11 ] o

(* pair *)

let test_pair_1 () =
  let o = ref [] in
  let s1 = Signal.make 1 in
  Signal.use (fun x -> push o (`s1 x)) s1;
  let s2 = Signal.make 100 in
  Signal.use (fun x -> push o (`s2 x)) s2;
  let s3 = Signal.pair s1 s2 in
  Signal.use (fun (x1, x2) -> push o (`s3 (x1 + x2))) s3;
  Signal.emit 2 s1;
  expect "pair_1" [ `s1 1; `s2 100; `s3 101; `s1 2; `s3 102 ] o

(* map2 *)

let test_map2_1 () =
  let o = ref [] in
  let s1 = Signal.make 1 in
  Signal.use (fun x -> push o (`s1 x)) s1;
  let s2 = Signal.make 100 in
  Signal.use (fun x -> push o (`s2 x)) s2;
  let s3 = Signal.map2 (fun x1 x2 -> x1 + x2) s1 s2 in
  Signal.use (fun x -> push o (`s3 x)) s3;
  Signal.emit 2 s1;
  expect "pair_1" [ `s1 1; `s2 100; `s3 101; `s1 2; `s3 102 ] o

(* syntax *)

let test_syntax_1 () =
  let o = ref [] in
  let s1 = Signal.make 1 in
  Signal.use (fun x -> push o (`s1 x)) s1;
  let s2 = Signal.make 100 in
  Signal.use (fun x -> push o (`s2 x)) s2;
  let s3 =
    let open Signal.Syntax in
    let+ x1 = s1 and+ x2 = s2 in
    x1 + x2
  in
  Signal.use (fun x3 -> push o (`s3 x3)) s3;
  Signal.emit 2 s1;
  expect "syntax_1" [ `s1 1; `s2 100; `s3 101; `s1 2; `s3 102 ] o

let test_sample_1 () =
  let o = ref [] in
  let s1 = Signal.make () in
  let s2 = Signal.make 20 in
  let s3 = Signal.sample ~on:s1 s2 in
  Signal.emit () s1;
  Signal.use (push o) s3;
  Signal.emit () s1;
  Signal.emit 21 s2;
  Signal.emit 22 s2;
  Signal.emit 23 s2;
  Signal.emit () s1;
  expect "sample_1" [ 20; 20; 23 ] o

(* TODO: test update, set, use2. *)

let run () =
  test_get_1 ();
  test_get_2 ();
  test_use_1 ();
  test_use_2 ();
  test_sub_1 ();
  test_sub_2 ();
  test_emit_1 ();
  test_emit_2 ();
  test_emit_3 ();
  test_map_1 ();
  test_map_2 ();
  test_map_3 ();
  test_map_4 ();
  test_map2_1 ();
  test_filter_map_1 ();
  test_tap_1 ();
  test_map_emit_1 ();
  test_map_emit_2 ();
  test_map_emit_3 ();
  test_map_emit_4 ();
  test_filter_1 ();
  test_filter_2 ();
  test_filter_3 ();
  test_reduce_1 ();
  test_reduce_2 ();
  test_select_1 ();
  test_select_2 ();
  test_select_3 ();
  test_uniq_1 ();
  test_pair_1 ();
  test_syntax_1 ();
  test_sample_1 ()
