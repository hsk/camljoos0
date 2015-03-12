(** Compiler phase to calculate resource information,
    such as JVM signatures and local variable indices.
*)

module TAst = Typecheckingast
module RAst = Resourceast


module LocalsEnv = Map.Make(String)

type info = { next_index : int;         (* OBS: intraprocedural info *)
              lenv : int LocalsEnv.t }


let rec res_lvalue info lvalue = 

  let pos = lvalue.TAst.lvalue_pos in
  let lvalue_type = lvalue.TAst.lvalue_type in
  let mklvalue lvalue =
    { RAst.lvalue_pos = pos; lvalue = lvalue; lvalue_type = lvalue_type }
  in
  match lvalue.TAst.lvalue with
  | TAst.Local id ->
    mklvalue (RAst.Local (id, LocalsEnv.find id.Ast.identifier info.lenv))
  | TAst.Field (id, ftyp) ->
    mklvalue (RAst.Field (id,ftyp))


and res_exp info exp =

  let pos = exp.TAst.exp_pos in
  let exp_type = exp.TAst.exp_type in
  let mkexp exp =
    { RAst.exp_pos = pos; exp = exp; exp_type = exp_type }
  in
  match exp.TAst.exp with
    | TAst.Binop (e1, op, e2) ->
      let e1 = res_exp info e1 in
      let e2 = res_exp info e2 in
      mkexp (RAst.Binop (e1, op, e2))
    | TAst.Unop (op, e)   -> mkexp (RAst.Unop (op, res_exp info e))
    | TAst.IntConst i     -> mkexp (RAst.IntConst i)
    | TAst.StringConst s  -> mkexp (RAst.StringConst s)
    | TAst.BooleanConst b -> mkexp (RAst.BooleanConst b)
    | TAst.Null           -> mkexp RAst.Null
    | TAst.This           -> mkexp RAst.This
    | TAst.Invoke (e,id,es,mtyp) ->
      let e = res_exp info e in
      mkexp (RAst.Invoke (e, id, List.map (res_exp info) es, mtyp))
    | TAst.New (t,es,contyp) ->
      mkexp (RAst.New (t, List.map (res_exp info) es, contyp))
    | TAst.Lvalue lvalue -> mkexp (RAst.Lvalue (res_lvalue info lvalue))
    | TAst.Assignment (lvalue, e) ->
      let lvalue = res_lvalue info lvalue in
      mkexp (RAst.Assignment (lvalue, res_exp info e))
    | TAst.Print e -> mkexp (RAst.Print (res_exp info e))
    | TAst.Read -> mkexp RAst.Read


let rec res_stm info stm = 
  let pos = stm.TAst.stm_pos in
  let mkstm stm =
    { RAst.stm_pos = pos; RAst.stm     = stm }
  in
  match stm.TAst.stm with
    | TAst.Exp e -> mkstm (RAst.Exp (res_exp info e))
    | TAst.IfThen (e, s) -> 
      let e = res_exp info e in
      let s = res_stm info s in
      mkstm (RAst.IfThen (e, s))
    | TAst.IfThenElse (e, s1, s2) -> 
      let e = res_exp info e in
      let s1 = res_stm info s1 in
      let s2 = res_stm info s2 in
      mkstm (RAst.IfThenElse (e, s1, s2))
    | TAst.While (e, s) -> 
      let e = res_exp info e in
      let s = res_stm info s in
      mkstm (RAst.While (e, s))
    | TAst.Empty -> mkstm (RAst.Empty)
    | TAst.Block stms -> mkstm (RAst.Block (List.map (res_stm info) stms))


let res_return_stm info retstm = 
  let pos = retstm.TAst.return_stm_pos in
  let mkretstm stm =
    { RAst.return_stm_pos = pos; return_stm = stm }
  in
  match retstm.TAst.return_stm with
  | TAst.VoidReturn    -> mkretstm (RAst.VoidReturn)
  | TAst.ValueReturn e -> mkretstm (RAst.ValueReturn (res_exp info e))


let res_formal_param info formal =      
  let (typ,id)   = formal in
  let next_index = info.next_index in
  let formal     = (typ, id, next_index) in
  let info       = { next_index = next_index + 1;  (* add another local *)
                     lenv = LocalsEnv.add id.Ast.identifier next_index info.lenv } in
  (formal, info)


let res_local info ldecl =      
  let (typ,id,exp) = ldecl in
  let exp          = res_exp info exp in
  let next_index   = info.next_index in
  let formal       = (typ, id, exp, next_index) in
  let info         = { next_index = next_index + 1;  (* add another local *)
                       lenv = LocalsEnv.add id.Ast.identifier next_index info.lenv } in
  (formal, info)


let res_field tdinfo fdecl =
  let info = { next_index = 0; (* Note: no locals in opt. field initializer *)
               lenv = LocalsEnv.empty } in
  { RAst.field_type      = fdecl.TAst.field_type;
    RAst.field_name      = fdecl.TAst.field_name;
    RAst.field_init      = Utils.opt (res_exp info) fdecl.TAst.field_init;
    RAst.field_signature = tdinfo ^ "/" ^ fdecl.TAst.field_name.Ast.identifier;
  }


let res_body fab =
  (* All methods are non-static, so first local variable has index 1*)
  let info = { next_index = 1;
               lenv       = LocalsEnv.empty } in
  (* The formals are traversed first, so they are
     assigned the lowest indices (as they should). *)
  let formals, info = Utils.fold res_formal_param info fab.TAst.formals in
  let locals, info  = Utils.fold res_local info fab.TAst.locals in
  { RAst.formals    = formals;
    RAst.locals     = locals;
    RAst.statements = List.map (res_stm info) fab.TAst.statements;
    RAst.return     = res_return_stm info fab.TAst.return; }

let make_method_sig tdinfo name formals return_sig =
  let formal_sigs = List.map (fun (t,_) -> Types.typeexp_to_sig t) formals in
  tdinfo ^ "/" ^ name ^ "(" ^ (String.concat "" formal_sigs) ^ ")" ^ return_sig 


let res_constructor tdinfo cdecl = 
  let body = cdecl.TAst.constructor_body in
  { RAst.constructor_name      = cdecl.TAst.constructor_name;
    RAst.constructor_body      = res_body body;
    RAst.constructor_signature = make_method_sig tdinfo "<init>" body.TAst.formals "V"
  }


let res_method tdinfo mdecl =
  let body  = mdecl.TAst.method_body in
  let body' = res_body body in
  let name = mdecl.TAst.method_name in
  let return_sig = Types.typeexp_to_sig mdecl.TAst.method_return_type in
  let method_sig = 
    make_method_sig tdinfo name.Ast.identifier body.TAst.formals return_sig in
  { RAst.method_return_type = mdecl.TAst.method_return_type;
    RAst.method_name        = name;
    RAst.method_body        = body';
    RAst.method_signature   = method_sig; }


let rec res_program prog =
  List.map (fun cdecl ->
    let class_name = cdecl.TAst.class_name in
    let tdinfo     = class_name.Ast.identifier in
    { RAst.source_file_name     = cdecl.TAst.source_file_name;
      RAst.class_name           = class_name;
      RAst.class_fields         = List.map (res_field tdinfo) cdecl.TAst.class_fields;
      RAst.class_constructor    = res_constructor tdinfo cdecl.TAst.class_constructor;
      RAst.class_main           = Utils.opt res_body cdecl.TAst.class_main;
      RAst.class_methods        = List.map (res_method tdinfo) cdecl.TAst.class_methods;
      RAst.class_decl_signature = class_name.Ast.identifier;
    }
  ) prog

