(* Based on https://github.com/melange-re/melange/pull/396/files *)
module OCaml_Location = Location
open Ppxlib
module Helper = Ppxlib.Ast_helper

let pr fmt = Format.kasprintf (fun x -> prerr_endline x) fmt

module Builder = struct
  (* Ast_builder.Default assigns attributes to be the empty.
     This wrapper re-exports all used fns with attrs arg to override them. *)

  include Ast_builder.Default

  let pexp_apply ~loc ?(attrs = []) e args =
    let e = Ast_builder.Default.pexp_apply ~loc e args in
    { e with pexp_attributes = attrs }

  let value_binding ~loc ~pat ~expr ~attrs =
    let vb = Ast_builder.Default.value_binding ~loc ~pat ~expr in
    { vb with pvb_attributes = attrs }

  let value_description ~loc ~name ~type_ ~prim ~attrs =
    let vd = Ast_builder.Default.value_description ~loc ~name ~type_ ~prim in
    { vd with pval_attributes = attrs }
end

module Jsx_runtime = struct
  let elem name ~loc =
    Builder.pexp_ident ~loc
      { loc; txt = Ldot (Ldot (Lident "Jsx", "Elem"), name) }

  let attr name ~loc =
    Builder.pexp_ident ~loc
      { loc; txt = Ldot (Ldot (Lident "Jsx", "Attr"), name) }

  let null ~loc =
    Builder.pexp_ident ~loc { loc; txt = Ldot (Lident "Jsx", "null") }

  let text ~loc =
    Builder.pexp_ident ~loc { loc; txt = Ldot (Lident "Jsx", "text") }

  let int ~loc =
    Builder.pexp_ident ~loc { loc; txt = Ldot (Lident "Jsx", "int") }

  let float ~loc =
    Builder.pexp_ident ~loc { loc; txt = Ldot (Lident "Jsx", "float") }

  let fragment ~loc =
    Builder.pexp_ident ~loc { loc; txt = Ldot (Lident "Jsx", "fragment") }

  let syntax_option1 ~loc =
    Builder.pexp_ident ~loc
      { loc; txt = Ldot (Ldot (Lident "Jsx", "Attr"), "option1") }

  let syntax_option2 ~loc =
    Builder.pexp_ident ~loc
      { loc; txt = Ldot (Ldot (Lident "Jsx", "Attr"), "option2") }

  let syntax_option3 ~loc =
    Builder.pexp_ident ~loc
      { loc; txt = Ldot (Ldot (Lident "Jsx", "Attr"), "option3") }
end

let child_to_elem child =
  let loc = child.pexp_loc in
  match child.pexp_desc with
  (* "str" ==> Jsx.text("str") *)
  | Pexp_constant (Pconst_string _) ->
    Builder.pexp_apply ~loc (Jsx_runtime.text ~loc) [ (Nolabel, child) ]
  (* 42 ==> Jsx.int(42) *)
  | Pexp_constant (Pconst_integer _) ->
    Builder.pexp_apply ~loc (Jsx_runtime.int ~loc) [ (Nolabel, child) ]
  (* 3.14 ==> Jsx.float(3.14) *)
  | Pexp_constant (Pconst_float _) ->
    Builder.pexp_apply ~loc (Jsx_runtime.float ~loc) [ (Nolabel, child) ]
  | _ -> child

let children_to_ast_array ~loc ~mapper l0 =
  let rec loop l acc =
    match l.pexp_desc with
    (* [] *)
    | Pexp_construct ({ txt = Lident "[]"; _ }, None) ->
      Builder.pexp_array ~loc (List.rev_map child_to_elem acc)
    (* _::_ *)
    | Pexp_construct
        ({ txt = Lident "::"; _ }, Some { pexp_desc = Pexp_tuple [ x; l' ]; _ })
      -> loop l' (mapper#expression x :: acc)
    (* not a list, XXX: is this possible? *)
    | _not_a_list -> mapper#expression l
  in
  loop l0 []

let prop_to_apply ~loc:loc0 (arg_l, arg_e) =
  match (arg_l, arg_e) with
  (* ?l:(val_1, val_2) *)
  | ( Optional l
    , { pexp_desc = Pexp_tuple [ val_1; val_2 ]
      ; pexp_loc = loc
      ; pexp_attributes
      ; pexp_loc_stack = _
      } ) ->
    let syn_fun = Jsx_runtime.syntax_option2 ~loc in
    let syn_attr = Jsx_runtime.attr l ~loc in
    Builder.pexp_apply ~loc:loc0 syn_fun ~attrs:pexp_attributes
      [ (Nolabel, syn_attr); (Nolabel, val_1); (Nolabel, val_2) ]
  (* ?l:(val_1, val_2, val_3) *)
  | ( Optional l
    , { pexp_desc = Pexp_tuple [ val_1; val_2; val_3 ]
      ; pexp_loc = loc
      ; pexp_attributes
      ; pexp_loc_stack = _
      } ) ->
    let syn_fun = Jsx_runtime.syntax_option3 ~loc in
    let syn_attr = Jsx_runtime.attr l ~loc in
    Builder.pexp_apply ~loc:loc0 syn_fun ~attrs:pexp_attributes
      [ (Nolabel, syn_attr)
      ; (Nolabel, val_1)
      ; (Nolabel, val_2)
      ; (Nolabel, val_3)
      ]
  (* ?l:val_1 *)
  | Optional l, val_1 ->
    let syn_fun = Jsx_runtime.syntax_option1 ~loc:loc0 in
    let syn_attr = Jsx_runtime.attr l ~loc:loc0 in
    Builder.pexp_apply ~loc:loc0 syn_fun
      [ (Nolabel, syn_attr); (Nolabel, val_1) ]
  (* ~l *)
  | Labelled l, _ ->
    let attr_fun = Jsx_runtime.attr l ~loc:loc0 in
    Builder.pexp_apply ~loc:loc0 attr_fun [ (Nolabel, arg_e) ]
  | Nolabel, _ -> assert false

let prop_list_to_array ~loc (props : (arg_label * expression) list) =
  let propsApplies = List.map (prop_to_apply ~loc) props in
  Builder.pexp_array ~loc propsApplies

let extractChildren ?(removeLastPositionUnit = false) ~loc propsAndChildren =
  let rec allButLast_ lst acc =
    match lst with
    | [] -> []
    | [ ( Nolabel
        , { pexp_desc = Pexp_construct ({ txt = Lident "()"; _ }, None); _ } )
      ] -> acc
    | (Nolabel, _) :: _rest ->
      raise
        (Invalid_argument
           "JSX: found non-labelled argument before the last position")
    | arg :: rest -> allButLast_ rest (arg :: acc)
    [@@raises Invalid_argument]
  in
  let allButLast lst =
    allButLast_ lst [] |> List.rev
    [@@raises Invalid_argument]
  in
  match
    List.partition
      (fun (label, _) -> label = Labelled "children")
      propsAndChildren
  with
  | [], props ->
    (* no children provided? Place a placeholder list *)
    ( Builder.pexp_construct ~loc { loc; txt = Lident "[]" } None
    , if removeLastPositionUnit then allButLast props else props )
  | [ (_, childrenExpr) ], props ->
    (childrenExpr, if removeLastPositionUnit then allButLast props else props)
  | _ ->
    raise
      (Invalid_argument "JSX: somehow there's more than one `children` label")
  [@@raises Invalid_argument]

(* TODO: some line number might still be wrong *)
let rewritter =
  let transformUppercaseCall3 ~caller modulePath mapper loc attrs _
      callArguments =
    let children, argsWithLabels =
      extractChildren ~loc ~removeLastPositionUnit:true callArguments
    in
    let argsForMake = argsWithLabels in
    let children' = children_to_ast_array ~loc ~mapper children in
    let recursivelyTransformedArgsForMake =
      argsForMake
      |> List.map (fun (label, expression) ->
             (label, mapper#expression expression))
    in
    let props = recursivelyTransformedArgsForMake in
    let isCap str =
      let first = String.sub str 0 1 [@@raises Invalid_argument] in
      let capped = String.uppercase_ascii first in
      first = capped
      [@@raises Invalid_argument]
    in
    let ident =
      match modulePath with
      | Lident _ -> Ldot (modulePath, caller)
      | Ldot (_modulePath, value) as fullPath when isCap value ->
        Ldot (fullPath, caller)
      | modulePath -> modulePath
    in
    let propsArray = prop_list_to_array ~loc props in

    (* React.createElement(Component.make, props, ...children) *)
    Builder.pexp_apply ~loc ~attrs
      (Builder.pexp_ident ~loc { txt = ident; loc })
      [ (Nolabel, propsArray); (Nolabel, children') ]
    [@@raises Invalid_argument]
  in

  let transformLowercaseCall3 mapper loc attrs callArguments id =
    let children, nonChildrenProps =
      extractChildren ~removeLastPositionUnit:true ~loc callArguments
    in
    match (id, children.pexp_desc) with
    (* text/int/float *)
    | ( "text"
      , Pexp_construct
          ({ txt = Lident "::"; _ }, Some { pexp_desc = Pexp_tuple l; _ }) ) ->
      let elem_f = Jsx_runtime.text ~loc in
      let arg =
        match l with
        | [ x; _nil_exp ] -> x
        | _ ->
          Location.raise_errorf ~loc "%s requires exactly one child node" id
      in
      let args = [ (Nolabel, arg) ] in
      Builder.pexp_apply ~loc ~attrs elem_f args
    | ( "int"
      , Pexp_construct
          ({ txt = Lident "::"; _ }, Some { pexp_desc = Pexp_tuple l; _ }) ) ->
      let elem_f = Jsx_runtime.int ~loc in
      let arg =
        match l with
        | [ x; _nil_exp ] -> x
        | _ ->
          Location.raise_errorf ~loc "%s requires exactly one child node" id
      in
      let args = [ (Nolabel, arg) ] in
      Builder.pexp_apply ~loc ~attrs elem_f args
    | ( "float"
      , Pexp_construct
          ({ txt = Lident "::"; _ }, Some { pexp_desc = Pexp_tuple l; _ }) ) ->
      let elem_f = Jsx_runtime.float ~loc in
      let arg =
        match l with
        | [ x; _nil_exp ] -> x
        | _ ->
          Location.raise_errorf ~loc "%s requires exactly one child node" id
      in
      let args = [ (Nolabel, arg) ] in
      Builder.pexp_apply ~loc ~attrs elem_f args
    (* [@JSX] div(~children=[a]), coming from <div> a </div> *)
    | ( _
      , Pexp_construct
          ({ txt = Lident "::"; _ }, Some { pexp_desc = Pexp_tuple _; _ }) )
    | _, Pexp_construct ({ txt = Lident "[]"; _ }, None) ->
      let elem_f = Jsx_runtime.elem id ~loc in

      let props =
        nonChildrenProps
        |> List.map (fun (label, e) -> (label, mapper#expression e))
        |> prop_list_to_array ~loc
      in
      let children' = children_to_ast_array ~loc ~mapper children in
      let args =
        [ (* [|Jsx.Attr.className blabla; JsxAttr.foo bar|] *)
          (Nolabel, props)
        ; (* [|moreelem_fsHere|] *)
          (Nolabel, children')
        ]
      in
      Builder.pexp_apply
        ~loc (* throw away the [@JSX] attribute and keep the others, if any *)
        ~attrs (* React.createElement *)
        elem_f args
    (* [@JSX] div(~children= value), coming from <div> ...(value) </div> *)
    | _ ->
      raise
        (Invalid_argument
           "A spread as a DOM element's children don't make sense written \
            together. You can simply remove the spread.")
    [@@raises Invalid_argument]
  in

  let transform_jsx_apply mapper f_exp f_args attrs =
    match f_exp.pexp_desc with
    | Pexp_ident caller -> (
      match caller with
      | { txt = Lident "createElement"; _ } ->
        raise
          (Invalid_argument
             "JSX: `createElement` should be preceeded by a module name.")
      (* Foo.createElement(~prop1=foo, ~prop2=bar, ~children=[], ()) *)
      | { loc; txt = Ldot (mod_path, ("createElement" | "make")) } ->
        transformUppercaseCall3 ~caller:"make" mod_path mapper loc attrs f_exp
          f_args
      (* div(~prop1=foo, ~prop2=bar, ~children=[bla], ()) *)
      | { loc; txt = Lident id } ->
        transformLowercaseCall3 mapper loc attrs f_args id
      (* Foo.bar(~prop1=foo, ~prop2=bar, ~children=[], ()) *)
      | { loc; txt = Ldot (mod_path, f_non_std) } ->
        transformUppercaseCall3 ~caller:f_non_std mod_path mapper loc attrs
          f_exp f_args
      | { txt = Lapply _; _ } ->
        (* don't think there's ever a case where this is reached *)
        raise
          (Invalid_argument
             "JSX: encountered a weird case while processing the code. Please \
              report this!"))
    | _ ->
      raise
        (Invalid_argument
           "JSX: `createElement` should be preceeded by a simple, direct \
            module name.")
    [@@raises Invalid_argument]
  in

  object (mapper)
    inherit Ast_traverse.map as super

    method! expression exp =
      match exp.pexp_desc with
      (* Look for (f args [@JSX]) *)
      | Pexp_apply (f_exp, f_args) -> (
        let jsx_attrs, other_attrs =
          List.partition
            (fun attr -> attr.attr_name.txt = "JSX")
            exp.pexp_attributes
        in
        match jsx_attrs with
        | [] -> super#expression exp
        | _ -> transform_jsx_apply mapper f_exp f_args other_attrs)
      (* Fragment: <>foo</> as [@JSX][foo] *)
      | Pexp_construct
          ({ txt = Lident "::"; loc; _ }, Some { pexp_desc = Pexp_tuple _; _ })
      | Pexp_construct ({ txt = Lident "[]"; loc }, None) -> (
        let jsx_attrs, other_attrs =
          List.partition
            (fun attr -> attr.attr_name.txt = "JSX")
            exp.pexp_attributes
        in
        match jsx_attrs with
        | [] -> super#expression exp
        | _ ->
          let children' = children_to_ast_array ~loc ~mapper exp in
          let args = [ (Nolabel, children') ] in
          Builder.pexp_apply ~loc ~attrs:other_attrs
            (Jsx_runtime.fragment ~loc)
            args)
      (* Other: use default mapper *)
      | _ -> super#expression exp
    [@@raises Invalid_argument]
  end

let () =
  Driver.register_transformation "helix-ppx" ~impl:rewritter#structure
    ~intf:rewritter#signature
