(** Compiler phase to generate Java bytecode for all
    method bodies.

    Code generation for a statement or local variable declaration
    leaves the stack height unchanged.

    Code generation for an expression pushes the result
    of the expression onto the stack if the type of
    the expression is non-void, or leaves the stack height
    unchanged if the type of the expression is void.
*)

type id = Ast.id
type t = Types.t
type formal_param = t * id * int (*NEW*)
(*type local_decl   = t * id (* * exp *) * int (*NEW*)*)

type body =
    { formals    : formal_param list;
(*    locals     : local_decl list; *)
      body       : Inst.instruction list; }

type field_decl =
  | Method of t * id * body * string
  | Main of body
  | Constructor of id * body * string
  | Field of t * id (*field_init : exp option;*) * string

type class_decl =
    { cfilename     : string;
      cname           : id;
      cfields         : field_decl list;
      csig : string (*NEW*) }

let f_method_sig_known msig numargs numreturns =
  { Inst.method_sig      = msig;
    Inst.method_nargs    = numargs;
    Inst.method_nreturns = numreturns
  }

let f_method_sig id base m =
  let mname   = id.Ast.id in
  let basesig = Types.cname_to_sig base in
  let argsigs = List.map Types.t_to_sig m.Types.method_formals in
  let ressig  = Types.t_to_sig m.Types.method_result in
  let numargs = List.length m.Types.method_formals in
  let numreturns = if m.Types.method_result = Types.Void then 0 else 1 in
  let msig = basesig ^ "/" ^ mname ^ "(" ^ (String.concat "" argsigs) ^ ")" ^ ressig in
  f_method_sig_known msig numargs numreturns

let f_constructor_sig base (name,formals) =
    let basesig = Types.cname_to_sig base in
    let argsigs = List.map Types.t_to_sig formals in
    let numargs = List.length formals in
    let msig    = basesig ^ "/<init>(" ^ (String.concat "" argsigs) ^ ")V" in
    f_method_sig_known msig numargs 0

let f_cond = function
  | Typing.Eq -> Inst.Eq
  | Typing.Ne -> Inst.Ne
  | Typing.Lt -> Inst.Lt
  | Typing.Le -> Inst.Le
  | Typing.Gt -> Inst.Gt
  | Typing.Ge -> Inst.Ge
  | Typing.Aeq -> Inst.Aeq
  | Typing.Ane -> Inst.Ane
  | _ -> raise (Error.InternalCompilerError "Illegal cond in binary operation")

type env = { tenv             : Types.class_type Types.M.t;
             class_type       : Types.class_type;
             nonstatic_fields : Res.field_decl list }

let rec f_lvalue_read env {Res.lvalue;lvalue_type} =
  match lvalue with
  | Res.Field (id, ftype) -> 
    let fieldname = id.Ast.id in
    let fieldtype = Types.t_to_sig lvalue_type in
    let basesig   = env.class_type.Types.cname in
    [Inst.Iaload 0;
     Inst.Igetfield (basesig ^ "/" ^ fieldname ^ " " ^ fieldtype)]
  | Res.Local (id, i) -> 
    begin match lvalue_type with
    | Types.Int
    | Types.Boolean -> [Inst.Iiload i] (* integer load *)
    | Types.String 
    | Types.Class _ -> [Inst.Iaload i] (* address load *)
    | _ -> raise (Error.InternalCompilerError "Illegal type of lvalue")
    end

and f_lvalue_write env {Res.lvalue;lvalue_type} =
  match lvalue with
  | Res.Field (id,base) ->
    let fieldname = id.Ast.id in
    let fieldtype = Types.t_to_sig lvalue_type in
    let basesig   = env.class_type.Types.cname in
    [Inst.Iaload(0);
     Inst.Iswap;
     Inst.Iputfield (basesig ^ "/" ^ fieldname ^ " " ^ fieldtype)]
  | Res.Local (id,i) -> 
    begin match lvalue_type with
    | Types.Int
    | Types.Boolean  -> [Inst.Iistore i] (* integer store *)
    | Types.String 
    | Types.Class _  -> [Inst.Iastore i] (* address store *)
    | _ -> raise (Error.InternalCompilerError "Illegal type of lvalue")
    end

and f_exp env {Res.exp;exp_type} =
  match exp with
  | Res.Binop (e1,op,e2) ->
    f_exp env e1 @
    f_exp env e2 @
    begin match op with
    | Typing.Add    -> [Inst.Iiadd]
    | Typing.Minus  -> [Inst.Iisub]
    | Typing.Times  -> [Inst.Iimul]
    | Typing.Divide -> [Inst.Iidiv]
    | Typing.Modulo -> [Inst.Iirem]
    | Typing.And    -> [Inst.Iiand]
    | Typing.Or     -> [Inst.Iior]
    | Typing.Xor    -> [Inst.Iixor]
    | Typing.Eq  | Typing.Ne    
    | Typing.Lt  | Typing.Le
    | Typing.Gt  | Typing.Ge
    | Typing.Aeq | Typing.Ane ->
      let truel = Inst.make_label "true" in
      let endl = Inst.make_label "end" in
      [ Inst.Iifcmp(f_cond op, truel);
        Inst.Ildc_int 0l;
        Inst.Igoto endl;
        Inst.Ilabel truel;
        Inst.Ildc_int 1l;
        Inst.Ilabel endl
      ]
    | Typing.Concat ->
      [ Inst.Iinvokevirtual (f_method_sig_known 
          "java/lang/String/concat(Ljava/lang/String;)Ljava/lang/String;" 1 1)
      ]
    end
  | Res.Unop (op, e) ->
    f_exp env e @
    begin match op with
    | Typing.Negate     -> [Inst.Iineg]
    | Typing.Complement ->
        let truel = Inst.make_label "true" in
        let endl = Inst.make_label "end" in
        [ Inst.Iif(Inst.Eq,truel);
          Inst.Ildc_int 0l;
          Inst.Igoto endl;
          Inst.Ilabel truel;
          Inst.Ildc_int 1l;
          Inst.Ilabel endl]
    | Typing.BooleanToString ->
      let msig = "java/lang/String/valueOf(Z)Ljava/lang/String;" in
      [Inst.Iinvokestatic (f_method_sig_known msig 1 1)]
    | Typing.IntToString ->
      let msig = "java/lang/String/valueOf(I)Ljava/lang/String;" in
      [Inst.Iinvokestatic (f_method_sig_known msig 1 1)]
    | Typing.CharToString ->
      let msig = "java/lang/String/valueOf(C)Ljava/lang/String;" in
      [Inst.Iinvokestatic (f_method_sig_known msig 1 1)]
    | Typing.ObjectToString ->
      let msig = "java/lang/String/valueOf(Ljava/lang/Object;)Ljava/lang/String;" in
      [Inst.Iinvokestatic (f_method_sig_known msig 1 1)]
    end
  | Res.IntConst i     -> [Inst.Ildc_int i]
  | Res.StringConst s  -> [Inst.Ildc_string s]
  | Res.BooleanConst b -> [Inst.Ildc_int (if b then 1l else 0l)]
  | Res.Null           -> [Inst.Iaconst_null]
  | Res.This           -> [Inst.Iaload 0]
  | Res.Invoke (e, id, es, mtype) ->
    begin
      f_exp env e @
      List.concat (List.map (fun e -> f_exp env e) es) @
      Inst.Iinvokevirtual (f_method_sig id (Types.t_to_string e.Res.exp_type) mtype) ::
      []
    end
  | Res.New (typ,es,c) ->
    let typesig = Types.t_to_string exp_type in
    begin
      Inst.Inew typesig ::
      Inst.Idup ::
      List.concat (List.map (fun e -> f_exp env e) es) @
      Inst.Iinvokespecial (f_constructor_sig typesig c) ::
      []
    end
  | Res.Lvalue lval -> f_lvalue_read env lval
  | Res.Assignment (lval, e) ->
    begin
      f_exp env e @
      Inst.Idup ::
      f_lvalue_write env lval
    end
  | Res.Print e ->
    let argtype =
      match e.Res.exp_type with
      | Types.Int -> "I"
      | Types.Boolean -> "Z"
      | Types.String
      | Types.Class _ -> "Ljava/lang/Object;"
      | _ -> raise (Error.InternalCompilerError "Illegal print type")
    in
    let msig = "java/io/PrintStream/print(" ^ argtype ^ ")V" in
    begin
      f_exp env e @
      Inst.Igetstatic "java/lang/System/out Ljava/io/PrintStream;" ::
      Inst.Iswap ::
      Inst.Iinvokevirtual (f_method_sig_known msig 1 0) ::
      []
    end
  | Res.Read ->
    (* Generate code as if System.in.read() was called. *)
    [ Inst.Igetstatic "java/lang/System/in Ljava/io/InputStream;";
      Inst.Iinvokevirtual (f_method_sig_known "java/io/InputStream/read()I" 0 1)
    ]

let rec f_stm env {Res.stm} =
  match stm with
  | Res.Exp e ->
    let ie = f_exp env e in
    begin match e.Res.exp_type with
    | Types.Void -> ie
    | _ -> ie @ [Inst.Ipop]
    end
  | Res.IfThen (e,s) ->
    let falsel = Inst.make_label "false" in
    begin
      f_exp env e @
      Inst.Iif (Inst.Eq,falsel) ::
      f_stm env s @
    Inst.Ilabel falsel ::
      []
    end
  | Res.IfThenElse (e,s1,s2) ->
    let falsel = Inst.make_label "false" in
    let endifl = Inst.make_label "endif" in
    begin
      f_exp env e @
      Inst.Iif (Inst.Eq,falsel) ::
      f_stm env s1 @
      Inst.Igoto endifl ::
    Inst.Ilabel falsel ::
      f_stm env s2 @
    Inst.Ilabel endifl ::
      []
    end
  | Res.While (e,s) ->
    let loopl = Inst.make_label "loop" in
    let condl = Inst.make_label "cond" in
    begin
      Inst.Igoto condl ::
    Inst.Ilabel loopl ::
      f_stm env s @
    Inst.Ilabel condl ::
      f_exp env e @
      Inst.Iif (Inst.Ne,loopl) ::
      []
    end
  | Res.Empty -> []
  | Res.Block b -> List.concat (List.map (f_stm env) b)

let f_return_stm env {Res.return_stm} =
  match return_stm with
  | Res.VoidReturn -> [Inst.Ireturn]
  | Res.ValueReturn e ->
    f_exp env e @
    begin match e.Res.exp_type with
    | Types.Int
    | Types.Boolean -> [Inst.Iireturn]
    | Types.String
    | Types.Class _
    | Types.Null -> [Inst.Iareturn]
    | Types.Void -> raise (Error.InternalCompilerError "Illegal type of return expression")
    end

let f_local env (typ, id, exp, offset) =
  match typ with
  | Types.Int
  | Types.Boolean -> f_exp env exp @ [Inst.Iistore offset]  (* integer store *)
  | Types.Class _
  | Types.String -> f_exp env exp @ [Inst.Iastore offset]  (* address store *)
  | Types.Void
  | Types.Null -> raise (Error.InternalCompilerError "Illegal type of local initializer")

let f_body env {Res.locals;stms;return;formals} =
  let locals = List.concat (List.map (f_local env) locals) in
  let stms   = List.concat (List.map (f_stm env) stms) in
  let return = f_return_stm env return in
  { formals; body = locals @ stms @ return }

let f_field env = function
  | Res.Method(return_type, name, body, signature) ->
    Method (return_type, name, f_body env  body,  signature)
  | Res.Constructor(name, body, signature) ->
    let body = f_body env body in
    let supercall =
      [ Inst.Iaload 0;
        Inst.Iinvokespecial (f_method_sig_known "java/lang/Object/<init>()V" 0 0)]
    in
    let field_init =
        List.fold_right (fun field flist ->
          begin match field with
          | Res.Field(ty,name,init,signature) ->
            begin match init with
            | None -> flist
            | Some e -> 
              let fieldsig = signature ^ " " ^ (Types.t_to_sig ty) in
              begin
                Inst.Iaload 0 ::
                f_exp env e @
                Inst.Iputfield fieldsig ::
                flist
              end
            end
          | _ -> flist
          end
        ) env.nonstatic_fields []
    in
    let insts = supercall @ field_init @ body.body in
    Constructor (name, { body with body = insts }, signature)
  | Res.Main body -> Main (f_body env body)
  | Res.Field (ty, name, _, signature) -> Field(ty, name, signature)

let f tenv prog =
  List.map (fun {Res.cname;cfields;csig;cfilename} ->
    let class_type = 
      Env.lookup_env tenv "class" cname.Ast.id cname.Ast.id_pos in
    let env = { tenv; class_type; nonstatic_fields = cfields } in
    { 
      cfilename;
      cname;
      cfields         = List.map (f_field env) cfields;
      csig = csig;
    }
  ) prog

