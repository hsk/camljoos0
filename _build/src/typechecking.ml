(** Compiler phase to perform type checking of the program.
    It also performs type coercions and other transformations
    based on the types.
*)

module LAst = Linkingast
module TAst = Typecheckingast
module M = Types.M


let rec typeexp_to_string = function
  | Types.Void -> "void"
  | Types.Int  -> "int"
  | Types.Boolean -> "boolean"
  | Types.String  -> "String"
  | Types.Class name -> name
  | Types.Null -> "null"


let binop_to_string = function
  | Ast.Add   -> "plus"
  | Ast.Minus  -> "minus"
  | Ast.Times  -> "times"
  | Ast.Divide -> "divide"
  | Ast.Modulo -> "modulo"
  | Ast.Eq  -> "eq"
  | Ast.Ne  -> "ne"
  | Ast.Lt  -> "lt"
  | Ast.Le  -> "le"
  | Ast.Gt  -> "gt"
  | Ast.Ge  -> "ge"
  | Ast.And -> "and"
  | Ast.Or  -> "or"
  | Ast.Xor -> "xor"
  | Ast.Concat -> "+"


let unop_to_string = function
  | Ast.Negate -> "negate"
  | Ast.Complement -> "complement"
  | Ast.CharToString -> "+(char)"


let convert_binop = function
  | Ast.Add   -> TAst.Add
  | Ast.Minus  -> TAst.Minus
  | Ast.Times  -> TAst.Times
  | Ast.Divide -> TAst.Divide
  | Ast.Modulo -> TAst.Modulo
  | Ast.Eq     -> TAst.Eq
  | Ast.Ne     -> TAst.Ne
  | Ast.Lt     -> TAst.Lt
  | Ast.Le     -> TAst.Le
  | Ast.Gt     -> TAst.Gt
  | Ast.Ge     -> TAst.Ge
  | Ast.And    -> TAst.And
  | Ast.Or     -> TAst.Or
  | Ast.Xor    -> TAst.Xor
  | Ast.Concat -> TAst.Concat


(** Insert a conversion to string of the given expression.
      @param exp  the expression to coerce. 
*)
let coerce_to_string exp err =
  let mkunop uop = { exp with TAst.exp = TAst.Unop (uop, exp) } in

  match exp.TAst.exp_type with
    | Types.Void    -> err ()
    | Types.Int     -> mkunop TAst.IntToString
    | Types.Boolean -> mkunop TAst.BooleanToString
    | Types.String  -> 
       (* If the expression is not a constant or the result
          of a string concatenation or conversion, it may
          be null. Since null values must be coerced to the
          string "null", we must insert a conversion to string
          even though the value is already a string. *)
      (match exp.TAst.exp with
        | TAst.StringConst _
        | TAst.Binop _
        | TAst.Unop _ -> exp
        | _           -> mkunop TAst.ObjectToString)
    | Types.Class _
    | Types.Null   -> mkunop TAst.ObjectToString
      (* Null values are not subject to constant folding, so
         just replacing by the string "null" could lead to
         incorrect results. *)


let assignable totype fromtype =
  if totype = fromtype then true else
  match (fromtype, totype) with
    | (Types.Null, Types.Class _)
    | (Types.Null, Types.String) -> true
    | _ -> false


let rec lookup_method mname pos = function
  | [] ->  Error.error pos ("The method '" ^ mname ^ "' is not defined.")
  | meth::meths ->
    if meth.Types.method_name = mname
    then meth
    else lookup_method mname pos meths


let binop_type_error lexp binop rexp pos =
  Error.error pos ("The operator " ^ (binop_to_string binop) ^
     " cannot be used with the types " ^ (typeexp_to_string lexp.TAst.exp_type) ^
                              " and " ^ (typeexp_to_string rexp.TAst.exp_type))
    

let unop_type_error unop exp pos =
  Error.error pos ("The operator " ^ (unop_to_string unop) ^
        " cannot be used with the type " ^ (typeexp_to_string exp.TAst.exp_type))


let assign_type_error totype fromtype pos =
  Error.error pos ("The type " ^ (typeexp_to_string fromtype) ^
                 " cannot be assigned to the type " ^ (typeexp_to_string totype))


let tcheck_lvalue lvalue tenv ctype lenv = 
  let pos = lvalue.Ast.lvalue_pos in

  let mklval lval typ = 
    { TAst.lvalue_pos = pos; TAst.lvalue_type = typ; TAst.lvalue = lval } in

  match lvalue.Ast.lvalue with
    | Ast.Local id ->
      let pos  = id.Ast.identifier_pos in
      let ltyp = Env.lookup_env lenv "variable" id.Ast.identifier pos in
      mklval (TAst.Local id) ltyp
    | Ast.Field f  ->
      let fname  = f.Ast.identifier in
      let fields = ctype.Types.class_fields in
      let ftyp   = List.find (fun ft -> ft.Types.field_name = fname) fields in
      mklval (TAst.Field (f,ftyp)) ftyp.Types.field_type


let rec tcheck_arg_list totypes fromtypes kind pos = 
  match (totypes, fromtypes) with
    | ([],[]) -> ()
    | (totyp::ttyps,fromtyp::ftyps) ->
      if assignable totyp fromtyp
      then tcheck_arg_list ttyps ftyps kind pos
      else assign_type_error totyp fromtyp pos
    | _ -> 
      Error.error pos ("Incorrect number of arguments for " ^ kind)


let rec tcheck_exp tenv ctype lenv exp = 
  let pos = exp.Ast.exp_pos in

  let mkexp exp typ = 
    { TAst.exp_pos = pos; TAst.exp_type = typ; TAst.exp = exp } in

  match exp.Ast.exp with
    | Ast.Binop (exp1,op,exp2) ->
      let exp1 = tcheck_exp tenv ctype lenv exp1 in
      let exp2 = tcheck_exp tenv ctype lenv exp2 in
      (match (exp1.TAst.exp_type, op, exp2.TAst.exp_type) with
        (* Only occurs with + (char) exp
           Both arguments must be of string type *)
        | (Types.String,   Ast.Concat, Types.String) ->
          mkexp (TAst.Binop (exp1,TAst.Concat,exp2)) Types.String
        (* String concatenation *)
        | (Types.String,   Ast.Add,   _           )
        | (_,              Ast.Add,   Types.String) ->
          let err () = binop_type_error exp1 op exp2 exp.Ast.exp_pos in
          let exp1 = coerce_to_string exp1 err in
          let exp2 = coerce_to_string exp2 err in
          mkexp (TAst.Binop (exp1,TAst.Concat,exp2)) Types.String
        (* Integer addition *)
        | (Types.Int,      Ast.Add,   Types.Int)
        (* Integer operation *)
        | (Types.Int,      Ast.Minus,  Types.Int)
        | (Types.Int,      Ast.Times,  Types.Int)
        | (Types.Int,      Ast.Divide, Types.Int)
        | (Types.Int,      Ast.Modulo, Types.Int) ->
          let op = convert_binop op in
          mkexp (TAst.Binop (exp1,op,exp2)) Types.Int

        | (Types.Int,      Ast.Eq,     Types.Int)
        | (Types.Int,      Ast.Ne,     Types.Int)
        | (Types.Boolean,  Ast.Eq,     Types.Boolean)
        | (Types.Boolean,  Ast.Ne,     Types.Boolean) ->
          let op = convert_binop op in
          mkexp (TAst.Binop (exp1,op,exp2)) Types.Boolean

        | (Types.String,   Ast.Eq,     Types.String)
        | (Types.String,   Ast.Eq,     Types.Null)
        | (Types.Class _,  Ast.Eq,     Types.Null)
        | (Types.Null,     Ast.Eq,     Types.String)
        | (Types.Null,     Ast.Eq,     Types.Class _)
        | (Types.Null,     Ast.Eq,     Types.Null) ->
          mkexp (TAst.Binop (exp1,TAst.Aeq,exp2)) Types.Boolean

        | (Types.String,   Ast.Ne,     Types.String)
        | (Types.String,   Ast.Ne,     Types.Null)
        | (Types.Class _,  Ast.Ne,     Types.Null)
        | (Types.Null,     Ast.Ne,     Types.String)
        | (Types.Null,     Ast.Ne,     Types.Class _)
        | (Types.Null,     Ast.Ne,     Types.Null) ->
          mkexp (TAst.Binop (exp1,TAst.Ane,exp2)) Types.Boolean

        | (Types.Class lc, Ast.Eq,     Types.Class rc) ->
          if lc <> rc
          then binop_type_error exp1 op exp2 exp.Ast.exp_pos
          else mkexp (TAst.Binop (exp1,TAst.Aeq,exp2)) Types.Boolean

        | (Types.Class lc, Ast.Ne,     Types.Class rc) ->
          if lc <> rc
          then binop_type_error exp1 op exp2 exp.Ast.exp_pos
          else mkexp (TAst.Binop (exp1,TAst.Ane,exp2)) Types.Boolean

        | (Types.Int,      Ast.Lt,     Types.Int)
        | (Types.Int,      Ast.Le,     Types.Int)
        | (Types.Int,      Ast.Gt,     Types.Int)
        | (Types.Int,      Ast.Ge,     Types.Int)

        | (Types.Boolean,  Ast.And,    Types.Boolean)
        | (Types.Boolean,  Ast.Or,     Types.Boolean)
        | (Types.Boolean,  Ast.Xor,    Types.Boolean) ->
          let op = convert_binop op in
          mkexp (TAst.Binop (exp1,op,exp2)) Types.Boolean
            
        | _ -> 
          binop_type_error exp1 op exp2 exp.Ast.exp_pos )

    | Ast.Unop (op,exp) ->
      let exp = tcheck_exp tenv ctype lenv exp in
      (match (op, exp.TAst.exp_type) with
        | (Ast.Negate,      Types.Int) -> 
          mkexp (TAst.Unop (TAst.Negate,exp)) Types.Int
        | (Ast.Complement,  Types.Boolean) -> 
          mkexp (TAst.Unop (TAst.Complement,exp)) Types.Boolean
        | (Ast.CharToString,Types.Int) ->
          mkexp (TAst.Unop (TAst.CharToString,exp)) Types.String
        | _ -> unop_type_error op exp pos)

    | Ast.IntConst i ->
      mkexp (TAst.IntConst i) Types.Int
    | Ast.StringConst s ->
      mkexp (TAst.StringConst s) Types.String
    | Ast.BooleanConst b ->
      mkexp (TAst.BooleanConst b) Types.Boolean
    | Ast.Null ->
      mkexp TAst.Null Types.Null
    | Ast.This ->
      mkexp TAst.This (Types.Class ctype.Types.class_name)
    | Ast.Invoke (exp,m,exps) ->
      let pos = exp.Ast.exp_pos in
      let exp  = tcheck_exp tenv ctype lenv exp in
      let exps = List.map (tcheck_exp tenv ctype lenv) exps in
      (match exp.TAst.exp_type with
        | Types.Class cid ->
          let pos     = m.Ast.identifier_pos in
          let ctype   = M.find cid tenv in
          let methods = ctype.Types.class_methods in
          let mtype   = lookup_method m.Ast.identifier pos methods in
          let formals = mtype.Types.method_formals in
          let actuals = List.map (fun e -> e.TAst.exp_type) exps in
          begin
            tcheck_arg_list formals actuals "method" pos;
            mkexp (TAst.Invoke (exp,m,exps,mtype)) mtype.Types.method_result;
          end
        | _ -> Error.error pos 
                      "Only class types can be method invocation receivers" )
    | Ast.New (id,exps) ->
      let pos         = id.Ast.identifier_pos in
      let exps        = List.map (tcheck_exp tenv ctype lenv) exps in
      let ctype       = M.find id.Ast.identifier tenv in
      let constructor = ctype.Types.class_constructor in
      let formals     = constructor.Types.constructor_formals in
      let actuals     = List.map (fun e -> e.TAst.exp_type) exps in
      tcheck_arg_list formals actuals "constructor" pos;
      mkexp (TAst.New (id,exps,constructor)) (Types.Class id.Ast.identifier)
      
    | Ast.Lvalue lval ->
      let lval = tcheck_lvalue lval tenv ctype lenv in
      mkexp (TAst.Lvalue lval) lval.TAst.lvalue_type
    | Ast.Assignment (lval,exp) ->
      let pos = exp.Ast.exp_pos in
      let lval = tcheck_lvalue lval tenv ctype lenv in
      let exp  = tcheck_exp tenv ctype lenv exp in
      let lvalue_type = lval.TAst.lvalue_type in
      let exp_type    = exp.TAst.exp_type in
      if not (assignable lvalue_type exp_type) then
        assign_type_error lvalue_type exp_type pos else
      mkexp (TAst.Assignment (lval,exp)) lvalue_type
    | Ast.Print e ->
      let pos = e.Ast.exp_pos in
      let e = tcheck_exp tenv ctype lenv e in
      if e.TAst.exp_type = Types.Void then
        Error.error pos "Value to be printed must not be void" else
      mkexp (TAst.Print e) Types.Void
    | Ast.Read ->
      mkexp TAst.Read Types.Int
  

let rec tcheck_stm tenv ctype lenv stm = 
  let pos = stm.Ast.stm_pos in

  let mkstm stm = 
    { TAst.stm_pos = pos; TAst.stm = stm } in

  match stm.Ast.stm with
    | Ast.Exp e -> mkstm (TAst.Exp (tcheck_exp tenv ctype lenv e))
    | Ast.IfThen (e, s) ->
      let e = tcheck_exp tenv ctype lenv e in
      if e.TAst.exp_type <> Types.Boolean then
        let pos = e.TAst.exp_pos in
        Error.error pos "Condition must have type boolean" else
      let s = tcheck_stm tenv ctype lenv s in
      mkstm (TAst.IfThen (e, s))
    | Ast.IfThenElse (e,s1,s2) ->
      let e  = tcheck_exp tenv ctype lenv e in
      if e.TAst.exp_type <> Types.Boolean then
        let pos = e.TAst.exp_pos in
        Error.error pos "Condition must have type boolean" else
      let s1 = tcheck_stm tenv ctype lenv s1 in
      let s2 = tcheck_stm tenv ctype lenv s2 in
      mkstm (TAst.IfThenElse (e, s1, s2))
    | Ast.While (e, s) ->
      let e = tcheck_exp tenv ctype lenv e in
      if e.TAst.exp_type <> Types.Boolean then
        let pos = e.TAst.exp_pos in
        Error.error pos "Condition must have type boolean" else
      let s = tcheck_stm tenv ctype lenv s in
      mkstm (TAst.While (e, s))
    | Ast.Empty -> mkstm TAst.Empty
    | Ast.Block stms -> mkstm (TAst.Block (List.map(tcheck_stm tenv ctype lenv) stms))


let tcheck_return_stm return_stm tenv ctype lenv rettype =  
  let pos = return_stm.Ast.return_stm_pos in

  let mkretstm retstm = 
    { TAst.return_stm_pos = pos; TAst.return_stm = retstm } in

  match return_stm.Ast.return_stm with
    | Ast.VoidReturn ->
      if rettype <> Types.Void then
        Error.error pos "A non-void method must return a value" else
      mkretstm TAst.VoidReturn
    | Ast.ValueReturn e ->
      let e = tcheck_exp tenv ctype lenv e in
      let etype = e.TAst.exp_type in
      if not (assignable rettype etype) then 
        assign_type_error rettype etype pos else
      mkretstm (TAst.ValueReturn e)



let tcheck_formal_param tenv lenv formal = 
  let (typ, id) = formal in
  (formal, M.add id.Ast.identifier typ lenv)


let tcheck_local tenv ctype lenv ldecl = 
  let (typ, id, exp) = ldecl in
  let exp   = tcheck_exp tenv ctype lenv exp in
  let etype = exp.TAst.exp_type in
  if not (assignable typ etype) then
    let pos = exp.TAst.exp_pos in
    assign_type_error typ etype pos else
  let lenv = M.add id.Ast.identifier typ lenv in
  ((typ, id, exp), lenv)


let tcheck_field tenv ctype fdecl =
  let exp_opt =
    match Utils.opt(tcheck_exp tenv ctype M.empty) fdecl.LAst.field_init with
    | None -> None
    | Some e ->
      let ftype = fdecl.LAst.field_type in
      let etype = e.TAst.exp_type in
      let pos   = e.TAst.exp_pos in
      if not (assignable ftype etype) then assign_type_error ftype etype pos else
      Some e
  in
  { TAst.field_type = fdecl.LAst.field_type;
    TAst.field_name = fdecl.LAst.field_name;
    TAst.field_init = exp_opt }


let tcheck_body tenv ctype rettype fab = 
  let (formals, lenv) = Utils.fold (tcheck_formal_param tenv) M.empty fab.LAst.formals in
  let (locals, lenv) = Utils.fold (tcheck_local tenv ctype) lenv fab.LAst.locals in
  { TAst.formals    = formals;
    TAst.locals     = locals;
    TAst.statements = List.map (tcheck_stm tenv ctype lenv) fab.LAst.statements;
    TAst.return     = tcheck_return_stm fab.LAst.return tenv ctype lenv rettype;
  }


let tcheck_constructor cdecl tenv ctype =
  let constructor_name = cdecl.LAst.constructor_name in
  if constructor_name.Ast.identifier <> ctype.Types.class_name then
    let pos = constructor_name.Ast.identifier_pos in
    Error.error pos "Constructor must have the same name as its enclosing class" else
  let body = cdecl.LAst.constructor_body in
  { TAst.constructor_name = cdecl.LAst.constructor_name;
    TAst.constructor_body = tcheck_body tenv ctype Types.Void body;
  }


let tcheck_method tenv ctype mdecl =
  let rettype = mdecl.LAst.method_return_type in
  { TAst.method_return_type = rettype;
    TAst.method_name        = mdecl.LAst.method_name;
    TAst.method_body        = tcheck_body tenv ctype rettype mdecl.LAst.method_body;
  }


let rec tcheck_program tenv prog =
  List.map (fun cdecl ->
    let classname = cdecl.LAst.class_name in
    let pos       = classname.Ast.identifier_pos in
    let expected_name = classname.Ast.identifier ^ ".java" in
    if expected_name <> Filename.basename cdecl.LAst.source_file_name then
      Error.error pos ("The public class '" ^ classname.Ast.identifier ^ "' " ^
        "must be declared in a file called " ^ expected_name) else
    let class_type = Env.lookup_env tenv "class" classname.Ast.identifier pos in
    { TAst.source_file_name  = cdecl.LAst.source_file_name;
      TAst.class_name        = classname;
      TAst.class_fields      = List.map (tcheck_field tenv class_type) cdecl.LAst.class_fields;
      TAst.class_constructor = tcheck_constructor cdecl.LAst.class_constructor tenv class_type;
      TAst.class_main        = Utils.opt (tcheck_body tenv class_type Types.Void) cdecl.LAst.class_main;
      TAst.class_methods     = List.map (tcheck_method tenv class_type) cdecl.LAst.class_methods;
    }
  ) prog

