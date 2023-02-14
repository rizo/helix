open Helix

let main () =
  let open Html in
  div []
    [
      (* Show.main (); *)
      (* Conditional_attr.main (); *)
      Each.main ();
    ]

module Js2_tests = struct
  type person = { name : string; age : int }

  let person_of_js person_js =
    let open Js.Decoder in
    let name = field person_js "name" string in
    let age = field person_js "age" int in
    { name; age }

  let person_to_js person =
    let open Js.Encoder in
    obj [ ("name", string person.name); ("age", int person.age) ]

  let () =
    let joe = { name = "Joe"; age = 43 } in
    assert (person_of_js (person_to_js joe) = joe)
end

module Js_tests = struct
  module Js = Helix_js
  module E = Js.Encoder
  module D = Js.Decoder

  let () =
    assert (D.int (E.int 42) = 42);
    assert (D.bool (E.bool true) = true);
    assert (D.string (E.string "hello") = "hello");
    assert (
      D.pair D.int D.string (E.pair E.int E.string (42, "foo")) = (42, "foo"));
    assert (D.nullable D.int (E.nullable E.int None) = None);
    assert (D.nullable D.int (E.optional E.int (Some 42)) = Some 42);
    assert (D.optional D.int (E.optional E.int None) = None);
    assert (D.optional D.int (E.optional E.int (Some 42)) = Some 42);
    assert (D.array D.int (E.array E.int [| 1; 2; 3 |]) = [| 1; 2; 3 |])

  type person = { name : string; age : int }

  let person_of_js js =
    let name = Js.Obj.get js "name" D.string in
    let age = Js.Obj.get js "age" D.int in
    { name; age }

  let person_to_js person =
    E.obj [ ("name", E.string person.name); ("age", E.int person.age) ]

  let () =
    let joe = { name = "Joe"; age = 43 } in
    assert (person_of_js (person_to_js joe) = joe)
end

let () =
  let open Stdweb in
  match Dom.Document.get_element_by_id "root" with
  | Some root -> Helix.render root (main ())
  | None -> failwith "no #app"
