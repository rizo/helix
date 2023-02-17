(* Based on https://github.com/melange-re/melange/pull/396/files *)
module OCaml_Location = Location
open Ppxlib
module Helper = Ppxlib.Ast_helper

let pr fmt = Format.kasprintf print_endline fmt

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
  module React = struct
    let createElement ~loc =
      Builder.pexp_ident ~loc
        { loc; txt = Ldot (Lident "React", "createElement") }

    let null ~loc =
      Builder.pexp_ident ~loc { loc; txt = Ldot (Lident "React", "null") }

    let componentLike ~loc =
      { loc; txt = Ldot (Lident "React", "componentLike") }
  end

  module ReactDOM = struct
    let domProps ~loc =
      Builder.pexp_ident ~loc
        { loc; txt = Ldot (Lident "ReactDOM", "domProps") }
  end

  module ReactDOMRe = struct
    let createDOMElementVariadic ~loc =
      Builder.pexp_ident ~loc { loc; txt = Ldot (Lident "Jsx", "elem") }

    let createElement ~loc =
      Builder.pexp_ident ~loc { loc; txt = Ldot (Lident "Jsx", "elem") }
  end

  module ReasonReact = struct
    let fragment ~loc =
      Builder.pexp_ident ~loc
        { loc; txt = Ldot (Lident "ReasonReact", "fragment") }
  end
end

let rec find_opt p = function
  | [] -> None
  | x :: l -> if p x then Some x else find_opt p l

let nolabel = Nolabel
let labelled str = Labelled str
let optional str = Optional str

let isOptional str =
  match str with
  | Optional _ -> true
  | _ -> false

let isLabelled str =
  match str with
  | Labelled _ -> true
  | _ -> false

let getLabel str =
  match str with
  | Optional str | Labelled str -> str
  | Nolabel -> ""

let optionIdent = Lident "option"

let constantString ~loc str =
  Builder.pexp_constant ~loc (Pconst_string (str, Location.none, None))

let safeTypeFromValue valueStr =
  let valueStr = getLabel valueStr in
  match String.sub valueStr 0 1 with
  | "_" -> "T" ^ valueStr
  | _ -> valueStr
  [@@raises Invalid_argument]

let keyType loc =
  Builder.ptyp_constr ~loc { loc; txt = optionIdent }
    [ Builder.ptyp_constr ~loc { loc; txt = Lident "string" } [] ]

type 'a children = ListLiteral of 'a | Exact of 'a
type componentConfig = { propsName : string }

(* if children is a list, convert it to an array while mapping each element. If not, just map over it, as usual *)
let transformChildrenIfListUpper ~loc ~mapper theList =
  let rec transformChildren_ theList accum =
    (* not in the sense of converting a list to an array; convert the AST
       reprensentation of a list to the AST reprensentation of an array *)
    match theList with
    | { pexp_desc = Pexp_construct ({ txt = Lident "[]"; _ }, None); _ } -> (
      match accum with
      | [ singleElement ] -> Exact singleElement
      | accum -> ListLiteral (Builder.pexp_array ~loc (List.rev accum)))
    | { pexp_desc =
          Pexp_construct
            ( { txt = Lident "::"; _ }
            , Some { pexp_desc = Pexp_tuple [ v; acc ]; _ } )
      ; _
      } -> transformChildren_ acc (mapper#expression v :: accum)
    | notAList -> Exact (mapper#expression notAList)
  in
  transformChildren_ theList []

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
      (fun (label, _) -> label = labelled "children")
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

let unerasableIgnore loc =
  { attr_name = { loc; txt = "warning" }
  ; attr_payload =
      PStr [ Builder.pstr_eval ~loc (constantString ~loc "-16") [] ]
  ; attr_loc = loc
  }

let merlinFocus =
  { attr_name = { loc = Location.none; txt = "merlin.focus" }
  ; attr_payload = PStr []
  ; attr_loc = Location.none
  }

(* Helper method to look up the [@react.component] attribute *)
let hasAttr { attr_name = loc; _ } = loc.txt = "react.component"

(* Helper method to filter out any attribute that isn't [@react.component] *)
let otherAttrsPure { attr_name = loc; _ } = loc.txt <> "react.component"

(* Iterate over the attributes and try to find the [@react.component] attribute *)
let hasAttrOnBinding { pvb_attributes; _ } =
  find_opt hasAttr pvb_attributes <> None

(* Finds the name of the variable the binding is assigned to, otherwise raises Invalid_argument *)
let getFnName binding =
  match binding with
  | { pvb_pat = { ppat_desc = Ppat_var { txt; _ }; _ }; _ } -> txt
  | _ ->
    raise (Invalid_argument "react.component calls cannot be destructured.")
  [@@raises Invalid_argument]

let makeNewBinding binding expression newName =
  match binding with
  | { pvb_pat = { ppat_desc = Ppat_var ppat_var; _ } as pvb_pat; _ } ->
    { binding with
      pvb_pat =
        { pvb_pat with ppat_desc = Ppat_var { ppat_var with txt = newName } }
    ; pvb_expr = expression
    ; pvb_attributes = [ merlinFocus ]
    }
  | _ ->
    raise (Invalid_argument "react.component calls cannot be destructured.")
  [@@raises Invalid_argument]

(* Lookup the value of `props` otherwise raise Invalid_argument error *)
let getPropsNameValue _acc (loc, exp) =
  match (loc, exp) with
  | ( { txt = Lident "props"; _ }
    , { pexp_desc = Pexp_ident { txt = Lident str; _ }; _ } ) ->
    { propsName = str }
  | { txt; _ }, _ ->
    raise
      (Invalid_argument
         ("react.component only accepts props as an option, given: "
         ^ Longident.last_exn txt))
  [@@raises Invalid_argument]

(* Lookup the `props` record or string as part of [@react.component] and store the name for use when rewriting *)
let getPropsAttr payload =
  let defaultProps = { propsName = "Props" } in
  match payload with
  | Some
      (PStr
        ({ pstr_desc =
             Pstr_eval ({ pexp_desc = Pexp_record (recordFields, None); _ }, _)
         ; _
         }
        :: _rest)) -> List.fold_left getPropsNameValue defaultProps recordFields
  | Some
      (PStr
        ({ pstr_desc =
             Pstr_eval
               ({ pexp_desc = Pexp_ident { txt = Lident "props"; _ }; _ }, _)
         ; _
         }
        :: _rest)) -> { propsName = "props" }
  | Some (PStr ({ pstr_desc = Pstr_eval (_, _); _ } :: _rest)) ->
    raise
      (Invalid_argument
         "react.component accepts a record config with props as an options.")
  | _ -> defaultProps
  [@@raises Invalid_argument]

(* Plucks the label, loc, and type_ from an AST node *)
let pluckLabelDefaultLocType (label, default, _, _, loc, type_) =
  (label, default, loc, type_)

(* Lookup the filename from the location information on the AST node and turn it into a valid module identifier *)
let filenameFromLoc (pstr_loc : Location.t) =
  let fileName =
    match pstr_loc.loc_start.pos_fname with
    | "" -> !OCaml_Location.input_name
    | fileName -> fileName
  in
  let fileName =
    try Filename.chop_extension (Filename.basename fileName)
    with Invalid_argument _ -> fileName
  in
  String.capitalize_ascii fileName

(* Build a string representation of a module name with segments separated by $ *)
let makeModuleName fileName nestedModules fnName =
  let fullModuleName =
    match (fileName, nestedModules, fnName) with
    (* TODO: is this even reachable? It seems like the fileName always exists *)
    | "", nestedModules, "make" -> nestedModules
    | "", nestedModules, fnName -> List.rev (fnName :: nestedModules)
    | fileName, nestedModules, "make" -> fileName :: List.rev nestedModules
    | fileName, nestedModules, fnName ->
      fileName :: List.rev (fnName :: nestedModules)
  in
  let fullModuleName = String.concat "$" fullModuleName in
  fullModuleName

(*
  AST node builders
  These functions help us build AST nodes that are needed when transforming a [@react.component] into a
  constructor and a props external
*)

(* Build an AST node representing all named args for the `external` definition for a component's props *)
let rec recursivelyMakeNamedArgsForExternal list args =
  match list with
  | (label, default, loc, interiorType) :: tl ->
    recursivelyMakeNamedArgsForExternal tl
      (Builder.ptyp_arrow ~loc label
         (match (label, interiorType, default) with
         (* ~foo=1 *)
         | label, None, Some _ ->
           { ptyp_desc = Ptyp_var (safeTypeFromValue label)
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
           when isOptional label -> type_
         (* ~foo=? *)
         | label, None, _ when isOptional label ->
           { ptyp_desc = Ptyp_var (safeTypeFromValue label)
           ; ptyp_loc = loc
           ; ptyp_loc_stack = []
           ; ptyp_attributes = []
           }
         (* ~foo *)
         | label, None, _ ->
           { ptyp_desc = Ptyp_var (safeTypeFromValue label)
           ; ptyp_loc_stack = []
           ; ptyp_loc = loc
           ; ptyp_attributes = []
           }
         | _label, Some type_, _ -> type_)
         args)
  | [] -> args

(* Build an AST node for the [@bs.obj] representing props for a component *)
let makePropsValue fnName loc namedArgListWithKeyAndRef propsType =
  let propsName = { txt = fnName ^ "Props"; loc } in
  let type_ =
    recursivelyMakeNamedArgsForExternal namedArgListWithKeyAndRef
      (Builder.ptyp_arrow ~loc nolabel
         (Builder.ptyp_constr ~loc { txt = Lident "unit"; loc } [])
         propsType)
  in
  let bsobj =
    Builder.attribute ~loc ~name:{ txt = "bs.obj"; loc } ~payload:(PStr [])
  in
  Builder.value_description ~loc ~name:propsName ~type_ ~prim:[ "" ]
    ~attrs:[ bsobj ]
  [@@raises Invalid_argument]

(* Build an AST node representing an `external` with the definition of the [@bs.obj] *)
let makePropsExternal fnName loc namedArgListWithKeyAndRef propsType =
  Builder.pstr_primitive ~loc
    (makePropsValue fnName loc namedArgListWithKeyAndRef propsType)

(* Build an AST node for the signature of the `external` definition *)
let makePropsExternalSig fnName loc namedArgListWithKeyAndRef propsType =
  { psig_loc = loc
  ; psig_desc =
      Psig_value (makePropsValue fnName loc namedArgListWithKeyAndRef propsType)
  }
  [@@raises Invalid_argument]

(* Build an AST node for the props name when converted to an object inside the function signature  *)
let makePropsName ~loc name =
  { ppat_desc = Ppat_var { txt = name; loc }
  ; ppat_loc = loc
  ; ppat_loc_stack = []
  ; ppat_attributes = []
  }

let makeObjectField loc (str, attrs, type_) =
  { pof_desc = Otag ({ loc; txt = str }, type_)
  ; pof_loc = loc
  ; pof_attributes = attrs
  }

(* Build an AST node representing a "closed" object representing a component's props *)
let makePropsType ~loc namedTypeList =
  Builder.ptyp_constr ~loc
    { txt = Ldot (Lident "Js", "t"); loc }
    [ Builder.ptyp_object ~loc
        (List.map (makeObjectField loc) namedTypeList)
        Closed
    ]

(* Builds an AST node for the entire `external` definition of props *)
let makeExternalDecl fnName loc namedArgListWithKeyAndRef namedTypeList =
  makePropsExternal fnName loc
    (List.map pluckLabelDefaultLocType namedArgListWithKeyAndRef)
    (makePropsType ~loc namedTypeList)
  [@@raises Invalid_argument]

(* TODO: some line number might still be wrong *)
let rewritter =
  let transformUppercaseCall3 ~caller modulePath mapper loc attrs _
      callArguments =
    let children, argsWithLabels =
      extractChildren ~loc ~removeLastPositionUnit:true callArguments
    in
    let argsForMake = argsWithLabels in
    let childrenExpr = transformChildrenIfListUpper ~loc ~mapper children in
    let recursivelyTransformedArgsForMake =
      argsForMake
      |> List.map (fun (label, expression) ->
             (label, mapper#expression expression))
    in
    let childrenArg = ref None in
    let args =
      recursivelyTransformedArgsForMake
      @ (match childrenExpr with
        | Exact children -> [ (labelled "children", children) ]
        | ListLiteral { pexp_desc = Pexp_array list; _ } when list = [] -> []
        | ListLiteral expression ->
          (* this is a hack to support react components that introspect into their children *)
          childrenArg := Some expression;
          [ (labelled "children", Jsx_runtime.React.null ~loc) ])
      @ [ (nolabel, Builder.pexp_construct ~loc { loc; txt = Lident "()" } None)
        ]
    in
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
    let propsIdent =
      match ident with
      | Lident path -> Lident (path ^ "Props")
      | Ldot (ident, path) -> Ldot (ident, path ^ "Props")
      | _ ->
        raise
          (Invalid_argument
             "JSX name can't be the result of function applications")
    in
    let props =
      Builder.pexp_apply ~attrs ~loc
        (Builder.pexp_ident ~loc { loc; txt = propsIdent })
        args
    in
    (* handle key, ref, children *)
    (* React.createElement(Component.make, props, ...children) *)
    match !childrenArg with
    | None ->
      Builder.pexp_apply ~loc ~attrs
        (Jsx_runtime.React.createElement ~loc)
        [ (nolabel, Builder.pexp_ident ~loc { txt = ident; loc })
        ; (nolabel, props)
        ]
    | Some children ->
      Builder.pexp_apply ~loc ~attrs
        (Jsx_runtime.ReactDOMRe.createElement ~loc)
        [ (nolabel, Builder.pexp_ident ~loc { txt = ident; loc })
        ; (nolabel, props)
        ; (nolabel, children)
        ]
    [@@raises Invalid_argument]
  in

  let transformLowercaseCall3 mapper loc attrs callArguments id =
    let children, nonChildrenProps = extractChildren ~loc callArguments in
    let componentNameExpr = constantString ~loc id in
    let childrenExpr = transformChildrenIfList ~loc ~mapper children in
    let createElementCall =
      match children with
      (* [@JSX] div(~children=[a]), coming from <div> a </div> *)
      | { pexp_desc =
            ( Pexp_construct
                ({ txt = Lident "::"; _ }, Some { pexp_desc = Pexp_tuple _; _ })
            | Pexp_construct ({ txt = Lident "[]"; _ }, None) )
        ; _
        } -> Jsx_runtime.ReactDOMRe.createDOMElementVariadic ~loc
      (* [@JSX] div(~children= value), coming from <div> ...(value) </div> *)
      | _ ->
        raise
          (Invalid_argument
             "A spread as a DOM element's children don't make sense written \
              together. You can simply remove the spread.")
    in
    let args =
      match nonChildrenProps with
      | [ _justTheUnitArgumentAtEnd ] ->
        [ (* "div" *)
          (nolabel, componentNameExpr)
        ; (* [|moreCreateElementCallsHere|] *)
          (nolabel, childrenExpr)
        ]
      | nonEmptyProps ->
        let props =
          nonEmptyProps
          |> List.map (fun (label, expression) ->
                 (label, mapper#expression expression))
        in

        let propsCall =
          Builder.pexp_apply ~loc (Jsx_runtime.ReactDOM.domProps ~loc) props
        in
        [ (* "div" *)
          (nolabel, componentNameExpr)
        ; (* ~props=ReactDOM.domProps(~className=blabla, ~foo=bar, ()) *)
          (labelled "props", propsCall)
        ; (* [|moreCreateElementCallsHere|] *)
          (nolabel, childrenExpr)
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
            [ (* "div" *)
              (nolabel, Jsx_runtime.ReasonReact.fragment ~loc)
            ; (* [|moreCreateElementCallsHere|] *)
              (nolabel, childrenExpr)
            ]
          in
          Builder.pexp_apply
            ~loc
              (* throw away the [@JSX] attribute and keep the others, if any *)
            ~attrs:nonJSXAttributes
            (* React.createElement *)
            (Jsx_runtime.ReactDOMRe.createElement ~loc)
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
