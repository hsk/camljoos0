module TAst = Typecheckingast


let rec const_exp exp = 
  (*  let pos = exp.TAst.exp_pos in*)

  let mkexp e = {exp with TAst.exp = e} in
  (*  let mkboolexp e = {exp with TAst.exp = e; TAst.exp_type = Types.Boolean} in*)

  match exp.TAst.exp with
  | TAst.Binop (exp1,op,exp2) ->
    let exp1 = const_exp exp1 in
    let exp2 = const_exp exp2 in
    (match (exp1.TAst.exp, exp2.TAst.exp) with
      | (TAst.IntConst left, TAst.IntConst right) ->
        (match op with
          | TAst.Add    -> mkexp (TAst.IntConst (Int32.add left right))
          | TAst.Minus  -> mkexp (TAst.IntConst (Int32.sub left right))
          | TAst.Times  -> mkexp (TAst.IntConst (Int32.mul left right))
          | TAst.Divide ->
            if right <> 0l
            then mkexp (TAst.IntConst (Int32.div left right))
            else exp
          | TAst.Modulo ->
            if right <> 0l
            then mkexp (TAst.IntConst (Int32.rem left right))
            else exp
          | TAst.Eq     -> mkexp (TAst.BooleanConst (left = right))
          | TAst.Ne     -> mkexp (TAst.BooleanConst (left <> right))
          | TAst.Lt     -> mkexp (TAst.BooleanConst (left < right))
          | TAst.Le     -> mkexp (TAst.BooleanConst (left <= right))
          | TAst.Gt     -> mkexp (TAst.BooleanConst (left > right))
          | TAst.Ge     -> mkexp (TAst.BooleanConst (left >= right))
          | _           ->
            raise (Error.InternalCompilerError "Invalid op for int folding") )
      | (TAst.BooleanConst left, TAst.BooleanConst right) ->
        (match op with
          | TAst.Eq     -> mkexp (TAst.BooleanConst (left = right))
          | TAst.Ne     -> mkexp (TAst.BooleanConst (left <> right))
          | TAst.And    -> mkexp (TAst.BooleanConst (left && right))
          | TAst.Or     -> mkexp (TAst.BooleanConst (left || right))
          | TAst.Xor    -> mkexp (TAst.BooleanConst (left <> right))
          | _           ->
            raise (Error.InternalCompilerError "Invalid op for boolean folding") )
      | (TAst.StringConst left, TAst.StringConst right) ->
        (match op with
          | TAst.Concat -> mkexp (TAst.StringConst  (left ^ right))
          | TAst.Aeq    -> mkexp (TAst.BooleanConst (left = right))
          | TAst.Ane    -> mkexp (TAst.BooleanConst (left <> right))
          | _           ->
            raise (Error.InternalCompilerError "Invalid op for string folding") )
      | (_,_) -> mkexp (TAst.Binop (exp1,op,exp2)))
  | TAst.Unop (op,exp) ->
    let exp = const_exp exp in
    (match (op, exp.TAst.exp) with
      | (TAst.Negate,          TAst.IntConst i)     ->
        mkexp (TAst.IntConst (Int32.neg i))
      | (TAst.Complement,      TAst.BooleanConst b) ->
        mkexp (TAst.BooleanConst (not b))
      | (TAst.BooleanToString, TAst.BooleanConst b) ->
        mkexp (TAst.StringConst (string_of_bool b))
      | (TAst.CharToString,    TAst.IntConst i)     ->
        mkexp (TAst.StringConst (String.make 1 (Char.chr (Int32.to_int i))))
      | (TAst.IntToString,     TAst.IntConst i)     -> 
        mkexp (TAst.StringConst (Int32.to_string i))
      | (TAst.ObjectToString,  _)
      | (_,                    _) -> 
        mkexp (TAst.Unop (op, exp)) )
  | TAst.StringConst s ->
    mkexp(TAst.StringConst (String.sub s 1 (String.length s - 2))) (*do not include ""*)
  | TAst.IntConst _
  | TAst.BooleanConst _
  | TAst.Null
  | TAst.This -> exp
  | TAst.Invoke (exp, id, exps, mtyp) ->
    let exp  = const_exp exp in
    mkexp (TAst.Invoke (exp, id, List.map const_exp exps, mtyp))
  | TAst.New (id,exps,constrtyp) -> mkexp (TAst.New (id,List.map const_exp exps,constrtyp))
  | TAst.Lvalue _ -> exp
  | TAst.Assignment (lvalue,exp) -> mkexp (TAst.Assignment (lvalue, const_exp exp))
  | TAst.Print exp -> mkexp (TAst.Print (const_exp exp))
  | TAst.Read -> exp


let rec const_stm stm = 
  let mkstm s = { stm with TAst.stm = s } in

  match stm.TAst.stm with
  | TAst.Exp exp -> mkstm (TAst.Exp (const_exp exp))
  | TAst.IfThen (exp,stm) ->
    let exp = const_exp exp in
    let stm = const_stm stm in
    mkstm (TAst.IfThen (exp,stm))
  | TAst.IfThenElse (exp,stm1,stm2) ->
    let exp  = const_exp exp in
    let stm1 = const_stm stm1 in
    let stm2 = const_stm stm2 in
    mkstm (TAst.IfThenElse (exp, stm1, stm2))
  | TAst.While (exp,stm) ->
    let exp = const_exp exp in
    let stm = const_stm stm in
    (match exp.TAst.exp with
      | TAst.BooleanConst b ->
        let pos = exp.TAst.exp_pos in
        Error.error pos "Constant while condition not allowed"
      | _ ->
        mkstm (TAst.While (exp, stm)))
  | TAst.Empty -> stm
  | TAst.Block stms -> mkstm (TAst.Block (List.map const_stm stms))


let const_return_stm retstm =
  match retstm.TAst.return_stm with
  | TAst.VoidReturn      -> retstm
  | TAst.ValueReturn exp ->
    { retstm with TAst.return_stm = TAst.ValueReturn (const_exp exp) }


let const_local ldecl =
  let (typ, id, exp) = ldecl in
  (typ, id, const_exp exp)


let const_field fdecl =
  { fdecl with TAst.field_init = Utils.opt const_exp fdecl.TAst.field_init }


let const_body fab =
  { TAst.formals    = fab.TAst.formals;
    TAst.locals     = List.map const_local fab.TAst.locals;
    TAst.statements = List.map const_stm fab.TAst.statements;
    TAst.return     = const_return_stm fab.TAst.return }


let const_constructor cdecl =
  { cdecl with TAst.constructor_body = const_body cdecl.TAst.constructor_body }


let const_method mdecl =
  { mdecl with TAst.method_body = const_body mdecl.TAst.method_body }


let rec const_program prog =
  List.map (fun cdecl ->
    { cdecl with
      TAst.class_name        = cdecl.TAst.class_name;
      TAst.class_fields      = List.map const_field cdecl.TAst.class_fields;
      TAst.class_constructor = const_constructor cdecl.TAst.class_constructor;
      TAst.class_main        = Utils.opt const_body cdecl.TAst.class_main;
      TAst.class_methods     = List.map const_method cdecl.TAst.class_methods;
    }
  ) prog

