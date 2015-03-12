(** Compiler phase to check the uses of all types and variables
    to their definitions.
*)

module LAst = Linkingast
module M  = Types.M


let link_typeexp texp tenv =
  match texp.Ast.typeexp with
  | Ast.Void     -> Types.Void
  | Ast.Int      -> Types.Int
  | Ast.Boolean  -> Types.Boolean
  | Ast.String   -> Types.String
  | Ast.Class id ->
    let classname = id.Ast.identifier in
    if not (M.mem classname tenv) then
      let pos = id.Ast.identifier_pos in
      Error.error pos ("The class '" ^ classname ^ "' is not defined.") else
    Types.Class classname


let link_lvalue tenv ctype lenv lvalue =
  match lvalue.Ast.lvalue with
  | Ast.Local id ->
    let var_name = id.Ast.identifier in
    if not (List.mem var_name lenv) then
      let pos = id.Ast.identifier_pos in
      Error.error pos ("The variable '" ^ var_name ^ "' is not defined.") else
    ()
  | Ast.Field f ->
    let field_name = f.Ast.identifier in
    let field_list = ctype.Types.class_fields in
    if not (List.exists (fun ftyp -> ftyp.Types.field_name = field_name) field_list) then
      let pos = f.Ast.identifier_pos in
      Error.error pos ("The field '" ^ field_name ^ "' is not defined.") else
    ()


let rec link_exp tenv ctype lenv exp =
  match exp.Ast.exp with
  | Ast.Binop (e0,b,e1) ->
      link_exp tenv ctype lenv e0;
      link_exp tenv ctype lenv e1
  | Ast.Unop (op,e) ->
      link_exp tenv ctype lenv e 
  | Ast.IntConst _
  | Ast.StringConst _
  | Ast.BooleanConst _
  | Ast.Null
  | Ast.This -> ()
  | Ast.Invoke (e,id,es) ->
      link_exp tenv ctype lenv e;
      List.iter (link_exp tenv ctype lenv) es
  | Ast.New (id, exps) ->
      let classname = id.Ast.identifier in
      if not (M.mem classname tenv) then
        let pos = id.Ast.identifier_pos in
        Error.error pos ("The class '" ^ classname ^ "' is not defined.") else
      List.iter (link_exp tenv ctype lenv) exps
  | Ast.Lvalue lvalue ->
      link_lvalue tenv ctype lenv lvalue
  | Ast.Assignment (lvalue,exp) ->
      link_lvalue tenv ctype lenv lvalue;
      link_exp tenv ctype lenv exp
  | Ast.Print e ->
      link_exp tenv ctype lenv e
  | Ast.Read -> ()


let rec link_stm tenv ctype lenv stm =
  match stm.Ast.stm with
  | Ast.Exp e ->
      link_exp tenv ctype lenv e
  | Ast.IfThen (e,s) ->
      link_exp tenv ctype lenv e;
      link_stm tenv ctype lenv s
  | Ast.IfThenElse (e,s1,s2) ->
      link_exp tenv ctype lenv e;
      link_stm tenv ctype lenv s1;
      link_stm tenv ctype lenv s2
  | Ast.While (e,s) ->
      link_exp tenv ctype lenv e;
      link_stm tenv ctype lenv s
  | Ast.Empty -> ()
  | Ast.Block stms ->
      List.iter (link_stm tenv ctype lenv) stms


let link_return_stm tenv ctype lenv retstm =
  match retstm.Ast.return_stm with
  | Ast.VoidReturn -> ()
  | Ast.ValueReturn e -> link_exp tenv ctype lenv e


let link_formal_param tenv (typ, id) =
  (id.Ast.identifier, (link_typeexp typ tenv, id))


let rec link_formal_params tenv lenv = function
  | [] -> (lenv,[])
  | formal::formals ->
    let (id, formal)   = link_formal_param tenv formal in 
    let (lenv,formals) = link_formal_params tenv (id::lenv) formals in
    (lenv,formal::formals)


let link_local tenv ctype lenv (typ,id,exp) =
  let typ = link_typeexp typ tenv in
  let ()  = link_exp tenv ctype lenv exp in
  (id.Ast.identifier, (typ, id, exp))


let rec link_locals tenv ctype lenv = function
  | [] -> (lenv,[])
  | ldecl::ldecls ->
    let (id, ldecl')    = link_local tenv ctype lenv ldecl in 
    let (lenv',ldecls') = link_locals tenv ctype (id::lenv) ldecls in
    (lenv',ldecl'::ldecls')


let link_field tenv ctype fdecl =
  let field_type = link_typeexp fdecl.Ast.field_type tenv in
  ignore (Utils.opt (link_exp tenv ctype []) fdecl.Ast.field_init); (*no locals*)
  { LAst.field_type = field_type;
    LAst.field_name = fdecl.Ast.field_name;
    LAst.field_init = fdecl.Ast.field_init; }


let link_body tenv ctype fab =
  let (lenv,formals) = link_formal_params tenv [] fab.Ast.formals in
  let (lenv, locals) = link_locals tenv ctype lenv fab.Ast.locals in
  List.iter (link_stm tenv ctype lenv) fab.Ast.statements;
  link_return_stm tenv ctype lenv fab.Ast.return;
  { LAst.formals    = formals;
    LAst.locals     = locals;
    LAst.statements = fab.Ast.statements;
    LAst.return     = fab.Ast.return; }


let link_constructor tenv ctype cdecl =
  { LAst.constructor_name = cdecl.Ast.constructor_name;
    LAst.constructor_body = link_body tenv ctype cdecl.Ast.constructor_body }


let link_method tenv ctype mdecl =
  { LAst.method_return_type = link_typeexp mdecl.Ast.method_return_type tenv;
    LAst.method_name        = mdecl.Ast.method_name;
    LAst.method_body        = link_body tenv ctype mdecl.Ast.method_body }


let rec link_program tenv prog =
  List.map (fun cdecl ->
    let class_type = M.find (cdecl.Ast.class_name.Ast.identifier) tenv in
    { LAst.source_file_name  = cdecl.Ast.source_file;
      LAst.class_name        = cdecl.Ast.class_name;
      LAst.class_fields      = List.map (link_field tenv class_type) cdecl.Ast.class_fields;
      LAst.class_constructor = link_constructor tenv class_type cdecl.Ast.class_constructor;
      LAst.class_main        = Utils.opt (link_body tenv class_type) cdecl.Ast.class_main;
      LAst.class_methods     = List.map (link_method tenv class_type) cdecl.Ast.class_methods;
    }
  ) prog

