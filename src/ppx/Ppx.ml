(* Based on https://github.com/melange-re/melange/pull/396/files *)
module OCaml_Location = Location
open Ppxlib
module Helper = Ppxlib.Ast_helper

let pr fmt = Format.kasprintf (fun x -> print_endline x) fmt

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

  let fragment ~loc =
    Builder.pexp_ident ~loc { loc; txt = Ldot (Lident "Jsx", "fragment") }
end

let getLabel str =
  match str with
  | Optional str | Labelled str -> str
  | Nolabel -> ""

(* if children is a list, convert it to an array while mapping each element. If not, just map over it, as usual *)
let children_to_ast_array ~loc ~mapper l0 =
  let rec loop l acc =
    match l with
    (* [] *)
    | { pexp_desc = Pexp_construct ({ txt = Lident "[]"; _ }, None); _ } -> (
      match acc with
      | [] -> None
      | _ -> Some (Builder.pexp_array ~loc (List.rev acc)))
    (* _::_ *)
    | { pexp_desc =
          Pexp_construct
            ( { txt = Lident "::"; _ }
            , Some { pexp_desc = Pexp_tuple [ x; l' ]; _ } )
      ; _
      } -> loop l' (mapper#expression x :: acc)
    (* not a list *)
    | not_a_list -> Some (mapper#expression not_a_list)
  in
  loop l0 []

let transformChildrenIfList ~loc ~mapper theList =
  let rec transformChildren_ theList accum =
    (* not in the sense of converting a list to an array; convert the AST
       reprensentation of a list to the AST reprensentation of an array *)
    match theList with
    | { pexp_desc = Pexp_construct ({ txt = Lident "[]"; _ }, None); _ } ->
      Builder.pexp_array ~loc (List.rev accum)
    | { pexp_desc =
          Pexp_construct
            ( { txt = Lident "::"; _ }
            , Some { pexp_desc = Pexp_tuple [ v; acc ]; _ } )
      ; _
      } -> transformChildren_ acc (mapper#expression v :: accum)
    | notAList -> mapper#expression notAList
  in
  transformChildren_ theList []

let transformPropToApply ~loc (arg_lab, exp) =
  let lab = getLabel arg_lab in
  [ Builder.pexp_apply ~loc (Jsx_runtime.attr lab ~loc) [ (Nolabel, exp) ] ]

let transformPropsToArray ~loc (props : (arg_label * expression) list) =
  let propsApplies = List.concat_map (transformPropToApply ~loc) props in
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

(*
  AST node builders
  These functions help us build AST nodes that are needed when transforming a [@react.component] into a
  constructor and a props external
*)

let __isOptional str =
  match str with
  | Optional _ -> true
  | _ -> false

let __safeTypeFromValue valueStr =
  let valueStr = getLabel valueStr in
  match String.sub valueStr 0 1 with
  | "_" -> "T" ^ valueStr
  | _ -> valueStr
  [@@raises Invalid_argument]

(* Build an AST node representing all named args for the `external` definition for a component's props *)
let rec __recursivelyMakeNamedArgsForExternal list args =
  match list with
  | (label, default, loc, interiorType) :: tl ->
    __recursivelyMakeNamedArgsForExternal tl
      (Builder.ptyp_arrow ~loc label
         (match (label, interiorType, default) with
         (* ~foo=1 *)
         | label, None, Some _ ->
           { ptyp_desc = Ptyp_var (__safeTypeFromValue label)
           ; ptyp_loc = loc
           ; ptyp_loc_stack = []
           ; ptyp_attributes = []
           }
         (* ~foo: int=1 *)
         | _label, Some type_, Some _ -> type_
         (* ~foo: option(int)=? *)
         | ( label
           , Some
               { ptyp_desc =
                   Ptyp_constr ({ txt = Lident "option"; _ }, [ type_ ])
               ; _
               }
           , _ )
         | ( label
           , Some
               { ptyp_desc =
                   Ptyp_constr
                     ({ txt = Ldot (Lident "*predef*", "option"); _ }, [ type_ ])
               ; _
               }
           , _ )
         (* ~foo: int=? - note this isnt valid. but we want to get a type error *)
         | label, Some type_, _
           when __isOptional label -> type_
         (* ~foo=? *)
         | label, None, _ when __isOptional label ->
           { ptyp_desc = Ptyp_var (__safeTypeFromValue label)
           ; ptyp_loc = loc
           ; ptyp_loc_stack = []
           ; ptyp_attributes = []
           }
         (* ~foo *)
         | label, None, _ ->
           { ptyp_desc = Ptyp_var (__safeTypeFromValue label)
           ; ptyp_loc_stack = []
           ; ptyp_loc = loc
           ; ptyp_attributes = []
           }
         | _label, Some type_, _ -> type_)
         args)
  | [] -> args

(* TODO: some line number might still be wrong *)
let rewritter =
  let transformUppercaseCall3 ~caller modulePath mapper loc attrs _
      callArguments =
    let children, argsWithLabels =
      extractChildren ~loc ~removeLastPositionUnit:true callArguments
    in
    let argsForMake = argsWithLabels in
    let childrenExpr = children_to_ast_array ~loc ~mapper children in
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
    let propsArray = transformPropsToArray ~loc props in
    (* handle key, ref, children *)
    (* React.createElement(Component.make, props, ...children) *)
    match childrenExpr with
    | None ->
      Builder.pexp_apply ~loc ~attrs
        (Builder.pexp_ident ~loc { txt = ident; loc })
        [ (Nolabel, propsArray) ]
    | Some children ->
      Builder.pexp_apply ~loc ~attrs
        (Builder.pexp_ident ~loc { txt = ident; loc })
        [ (Nolabel, propsArray); (Nolabel, children) ]
    [@@raises Invalid_argument]
  in

  let transformLowercaseCall3 mapper loc attrs callArguments id =
    let children, nonChildrenProps =
      extractChildren ~removeLastPositionUnit:true ~loc callArguments
    in
    let childrenExpr = children_to_ast_array ~loc ~mapper children in
    let createElementCall =
      match children with
      (* [@JSX] div(~children=[a]), coming from <div> a </div> *)
      | { pexp_desc =
            ( Pexp_construct
                ({ txt = Lident "::"; _ }, Some { pexp_desc = Pexp_tuple _; _ })
            | Pexp_construct ({ txt = Lident "[]"; _ }, None) )
        ; _
        } -> Jsx_runtime.elem id ~loc
      (* [@JSX] div(~children= value), coming from <div> ...(value) </div> *)
      | _ ->
        raise
          (Invalid_argument
             "A spread as a DOM element's children don't make sense written \
              together. You can simply remove the spread.")
    in
    let props =
      nonChildrenProps
      |> List.map (fun (label, expression) ->
             (label, mapper#expression expression))
      |> transformPropsToArray ~loc
    in
    let args =
      match childrenExpr with
      | None ->
        [ (* [|Jsx.Attr.className blabla; JsxAttr.foo bar|] *)
          (Nolabel, props)
        ]
      | Some children ->
        [ (* [|Jsx.Attr.className blabla; JsxAttr.foo bar|] *)
          (Nolabel, props)
        ; (* [|moreCreateElementCallsHere|] *)
          (Nolabel, children)
        ]
    in
    Builder.pexp_apply
      ~loc (* throw away the [@JSX] attribute and keep the others, if any *)
      ~attrs (* React.createElement *)
      createElementCall args
    [@@raises Invalid_argument]
  in

  let nestedModules = ref [] in

  let transformJsxCall mapper callExpression callArguments attrs =
    match callExpression.pexp_desc with
    | Pexp_ident caller -> (
      match caller with
      | { txt = Lident "createElement"; _ } ->
        raise
          (Invalid_argument
             "JSX: `createElement` should be preceeded by a module name.")
      (* Foo.createElement(~prop1=foo, ~prop2=bar, ~children=[], ()) *)
      | { loc; txt = Ldot (modulePath, ("createElement" | "make")) } ->
        transformUppercaseCall3 ~caller:"make" modulePath mapper loc attrs
          callExpression callArguments
      (* div(~prop1=foo, ~prop2=bar, ~children=[bla], ()) *)
      (* turn that into
         React.createElement(~props=ReactDOM.domProps(~props1=foo, ~props2=bar, ()), [|bla|]) *)
      | { loc; txt = Lident id } ->
        transformLowercaseCall3 mapper loc attrs callArguments id
      (* Foo.bar(~prop1=foo, ~prop2=bar, ~children=[], ()) *)
      (* Not only "createElement" or "make". See
         https://github.com/reasonml/reason/pull/2541 *)
      | { loc; txt = Ldot (modulePath, anythingNotCreateElementOrMake) } ->
        transformUppercaseCall3 ~caller:anythingNotCreateElementOrMake
          modulePath mapper loc attrs callExpression callArguments
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

    method! expression expression =
      match expression with
      (* Does the function application have the @JSX attribute? *)
      | { pexp_desc = Pexp_apply (callExpression, callArguments)
        ; pexp_attributes
        ; _
        } -> (
        let jsxAttribute, nonJSXAttributes =
          List.partition
            (fun { attr_name = attribute; _ } -> attribute.txt = "JSX")
            pexp_attributes
        in
        match (jsxAttribute, nonJSXAttributes) with
        (* no JSX attribute *)
        | [], _ -> super#expression expression
        | _, nonJSXAttributes ->
          transformJsxCall mapper callExpression callArguments nonJSXAttributes)
      (* is it a list with jsx attribute? Reason <>foo</> desugars to [@JSX][foo]*)
      | { pexp_desc =
            ( Pexp_construct
                ( { txt = Lident "::"; loc; _ }
                , Some { pexp_desc = Pexp_tuple _; _ } )
            | Pexp_construct ({ txt = Lident "[]"; loc }, None) )
        ; pexp_attributes
        ; _
        } as listItems -> (
        let jsxAttribute, nonJSXAttributes =
          List.partition
            (fun { attr_name = attribute; _ } -> attribute.txt = "JSX")
            pexp_attributes
        in
        match (jsxAttribute, nonJSXAttributes) with
        (* no JSX attribute *)
        | [], _ -> super#expression expression
        | _, nonJSXAttributes ->
          let childrenExpr = transformChildrenIfList ~loc ~mapper listItems in
          let args =
            [ (* [|moreCreateElementCallsHere|] *) (Nolabel, childrenExpr) ]
          in
          Builder.pexp_apply
            ~loc
              (* throw away the [@JSX] attribute and keep the others, if any *)
            ~attrs:nonJSXAttributes
            (Jsx_runtime.fragment ~loc)
            args)
      (* Delegate to the default mapper, a identity *)
      | e -> super#expression e
    [@@raises Invalid_argument]

    method! module_binding module_binding =
      (match module_binding.pmb_name.txt with
      | None -> ()
      | Some name -> nestedModules := name :: !nestedModules);
      let mapped = super#module_binding module_binding in
      let _ = nestedModules := List.tl !nestedModules in
      mapped
  end

let () =
  Driver.register_transformation "helix-ppx" ~impl:rewritter#structure
    ~intf:rewritter#signature
