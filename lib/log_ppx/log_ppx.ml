open Ppxlib

let expand ~ctxt binding expr =
  let open Ast_builder.Default in
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  let let_expr =
    pexp_apply ~loc
      (pexp_ident ~loc { txt = Ldot (Lident "Log", "log"); loc })
      [
        ( Nolabel,
          pexp_fun ~loc Nolabel None
            (ppat_construct ~loc { txt = Lident "()"; loc } None)
            binding.pvb_expr );
        (Nolabel, pexp_construct ~loc { txt = Lident "()"; loc } None);
      ]
  in
  pexp_letop ~loc
    (letop
       ~let_:
         (binding_op ~loc ~op:{ txt = "let+"; loc } ~pat:binding.pvb_pat
            ~exp:let_expr)
       ~ands:[] ~body:expr)

let log_extension =
  Extension.V3.declare "log" Extension.Context.Expression
    Ast_pattern.(single_expr_payload (pexp_let nonrecursive (__ ^:: nil) __))
    expand

let rule = Ppxlib.Context_free.Rule.extension log_extension
let () = Driver.register_transformation ~rules:[ rule ] "log"
