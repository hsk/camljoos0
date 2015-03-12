(** Compiler phase to generate Java bytecode for all
    method bodies.

    Code generation for a statement or local variable declaration
    leaves the stack height unchanged.

    Code generation for an expression pushes the result
    of the expression onto the stack if the type of
    the expression is non-void, or leaves the stack height
    unchanged if the type of the expression is void.
*)

module TAst = Typecheckingast
module RAst = Resourceast
module CAst = Codegenast
module Inst = Instruction


let codegen_method_sig_known msig numargs numreturns =
  { Inst.method_sig      = msig;
    Inst.method_nargs    = numargs;
    Inst.method_nreturns = numreturns
  }

let codegen_method_sig id base m =
  let mname   = id.Ast.identifier in
  let basesig = Types.cname_to_sig base in
  let argsigs = List.map Types.typeexp_to_sig m.Types.method_formals in
  let ressig  = Types.typeexp_to_sig m.Types.method_result in
  let numargs = List.length m.Types.method_formals in
  let numreturns = if m.Types.method_result = Types.Void then 0 else 1 in
  let msig = basesig ^ "/" ^ mname ^ "(" ^ (String.concat "" argsigs) ^ ")" ^ ressig in
  codegen_method_sig_known msig numargs numreturns

let codegen_constructor_sig base c =
    let basesig = Types.cname_to_sig base in
    let argsigs = List.map Types.typeexp_to_sig c.Types.constructor_formals in
    let numargs = List.length c.Types.constructor_formals in
    let msig    = basesig ^ "/<init>(" ^ (String.concat "" argsigs) ^ ")V" in
    codegen_method_sig_known msig numargs 0

let codegen_cond = function
  | TAst.Eq -> Inst.Eq
  | TAst.Ne -> Inst.Ne
  | TAst.Lt -> Inst.Lt
  | TAst.Le -> Inst.Le
  | TAst.Gt -> Inst.Gt
  | TAst.Ge -> Inst.Ge
  | TAst.Aeq -> Inst.Aeq
  | TAst.Ane -> Inst.Ane
  | _ -> raise (Error.InternalCompilerError "Illegal cond in binary operation")


type info = { tenv             : Types.class_type Types.M.t;
              class_type       : Types.class_type;
              nonstatic_fields : RAst.field_decl list }


let rec codegen_lvalue_read info lvalue =
  match lvalue.RAst.lvalue with
  | RAst.Field (id, ftype) -> 
    let fieldname = id.Ast.identifier in
    let fieldtype = Types.typeexp_to_sig lvalue.RAst.lvalue_type in
    let basesig   = info.class_type.Types.class_name in
    [Inst.Iaload 0;
     Inst.Igetfield (basesig ^ "/" ^ fieldname ^ " " ^ fieldtype)]
  | RAst.Local (id, i) -> 
    (match lvalue.RAst.lvalue_type with
      | Types.Int
      | Types.Boolean -> [Inst.Iiload i] (* integer load *)
      | Types.String 
      | Types.Class _ -> [Inst.Iaload i] (* address load *)
      | _ -> raise (Error.InternalCompilerError "Illegal type of lvalue") )


and codegen_lvalue_write info lvalue =
  match lvalue.RAst.lvalue with
    | RAst.Field (id,base) ->
      let fieldname = id.Ast.identifier in
      let fieldtype = Types.typeexp_to_sig lvalue.RAst.lvalue_type in
      let basesig   = info.class_type.Types.class_name in
      [Inst.Iaload(0);
       Inst.Iswap;
       Inst.Iputfield (basesig ^ "/" ^ fieldname ^ " " ^ fieldtype)]
    | RAst.Local (id,i) -> 
      (match lvalue.RAst.lvalue_type with
        | Types.Int
        | Types.Boolean  -> [Inst.Iistore i] (* integer store *)
        | Types.String 
        | Types.Class _  -> [Inst.Iastore i] (* address store *)
        | _ -> raise (Error.InternalCompilerError "Illegal type of lvalue") )


and codegen_exp info exp =
  match exp.RAst.exp with
  | RAst.Binop (e1,op,e2) ->
    let ie1 = codegen_exp info e1 in
    let ie2 = codegen_exp info e2 in
    (match op with
      | TAst.Add    -> List.concat [ie1; ie2; [Inst.Iiadd]]
      | TAst.Minus  -> List.concat [ie1; ie2; [Inst.Iisub]]
      | TAst.Times  -> List.concat [ie1; ie2; [Inst.Iimul]]
      | TAst.Divide -> List.concat [ie1; ie2; [Inst.Iidiv]]
      | TAst.Modulo -> List.concat [ie1; ie2; [Inst.Iirem]]
      | TAst.And    -> List.concat [ie1; ie2; [Inst.Iiand]]
      | TAst.Or     -> List.concat [ie1; ie2; [Inst.Iior]]
      | TAst.Xor    -> List.concat [ie1; ie2; [Inst.Iixor]]
      | TAst.Eq    
      | TAst.Ne    
      | TAst.Lt
      | TAst.Le
      | TAst.Gt
      | TAst.Ge
      | TAst.Aeq
      | TAst.Ane    ->
        let cond = codegen_cond op in
        let truel = Inst.make_label "true" in
        let endl = Inst.make_label "end" in
        List.concat [ie1;
                     ie2;
                     [Inst.Iifcmp(cond,truel);
                      Inst.Ildc_int 0l;
                      Inst.Igoto endl;
                      Inst.Ilabel truel;
                      Inst.Ildc_int 1l;
                      Inst.Ilabel endl]]
      | TAst.Concat ->
        List.concat [ie1;
                     ie2;
                     [Inst.Iinvokevirtual
                         (codegen_method_sig_known 
                            "java/lang/String/concat(Ljava/lang/String;)Ljava/lang/String;" 1 1)]]
    )
  | RAst.Unop (op,e) ->
    let ie = codegen_exp info e in
    (match op with
      | TAst.Negate     -> List.concat [ie; [Inst.Iineg]]
      | TAst.Complement ->
          let truel = Inst.make_label "true" in
          let endl = Inst.make_label "end" in
          List.concat [ie; [Inst.Iif(Inst.Eq,truel);
                            Inst.Ildc_int 0l;
                            Inst.Igoto endl;
                            Inst.Ilabel truel;
                            Inst.Ildc_int 1l;
                            Inst.Ilabel endl]]
      | TAst.BooleanToString ->
        let msig = "java/lang/String/valueOf(Z)Ljava/lang/String;" in
        List.concat [ie; 
                     [Inst.Iinvokestatic (codegen_method_sig_known msig 1 1) ]]
      | TAst.IntToString ->
        let msig = "java/lang/String/valueOf(I)Ljava/lang/String;" in
        List.concat [ie; 
                     [Inst.Iinvokestatic (codegen_method_sig_known msig 1 1) ]]
      | TAst.CharToString ->
        let msig = "java/lang/String/valueOf(C)Ljava/lang/String;" in
        List.concat [ie; 
                     [Inst.Iinvokestatic (codegen_method_sig_known msig 1 1) ]]
      | TAst.ObjectToString ->
        let msig = 
          "java/lang/String/valueOf(Ljava/lang/Object;)Ljava/lang/String;" in
        List.concat [ie; 
                     [Inst.Iinvokestatic (codegen_method_sig_known msig 1 1) ]]
    )
  | RAst.IntConst i     -> [Inst.Ildc_int i]
  | RAst.StringConst s  -> [Inst.Ildc_string s]
  | RAst.BooleanConst b -> [Inst.Ildc_int (if b then 1l else 0l)]
  | RAst.Null           -> [Inst.Iaconst_null]
  | RAst.This           -> [Inst.Iaload 0]
  | RAst.Invoke (e,id,es,mtype) ->
    let ie = codegen_exp info e in
    let ies = List.concat (List.map (fun e -> codegen_exp info e) es) in
    let recvtype = e.RAst.exp_type in
    let msig = (codegen_method_sig id (Types.typeexp_to_string recvtype) mtype) in
    let invokeinst = [Inst.Iinvokevirtual msig] in
    List.concat [ie;
                 ies;
                 invokeinst]
  | RAst.New (typ,es,c) ->
    let ies = List.concat (List.map (fun e -> codegen_exp info e) es) in
    let typesig = Types.typeexp_to_string exp.RAst.exp_type in
    List.concat [[Inst.Inew typesig;
                  Inst.Idup];
                 ies;
                 [Inst.Iinvokespecial (codegen_constructor_sig typesig c)]]
  | RAst.Lvalue lval ->
    codegen_lvalue_read info lval
  | RAst.Assignment (lval,e) ->
    let ie = codegen_exp info e in
    let writeinst = codegen_lvalue_write info lval in
    List.concat [ie;
                 [Inst.Idup];
                 writeinst]
  | RAst.Print e ->
    let ie      = codegen_exp info e in
    let argtype = (match e.RAst.exp_type with
      | Types.Int -> "I"
      | Types.Boolean -> "Z"
      | Types.String
      | Types.Class _ -> "Ljava/lang/Object;"
      | _ -> raise (Error.InternalCompilerError "Illegal print type") ) in
    let msig = "java/io/PrintStream/print(" ^ argtype ^ ")V" in
    List.concat [ie;
                 [Inst.Igetstatic "java/lang/System/out Ljava/io/PrintStream;";
                  Inst.Iswap;
                  Inst.Iinvokevirtual (codegen_method_sig_known msig 1 0)]]
  | RAst.Read ->
    (* Generate code as if System.in.read() was called. *)
    [Inst.Igetstatic "java/lang/System/in Ljava/io/InputStream;";
     Inst.Iinvokevirtual (codegen_method_sig_known "java/io/InputStream/read()I" 0 1)]


let rec codegen_stm info stm =
  match stm.RAst.stm with
  | RAst.Exp e ->
    let ie = codegen_exp info e in
    (match e.RAst.exp_type with
      | Types.Void -> ie
      | _ -> ie @ [Inst.Ipop] )
  | RAst.IfThen (e,s) ->
    let falsel = Inst.make_label "false" in
    let ie = codegen_exp info e in
    let is = codegen_stm info s in
    List.concat
      [ie;
       [Inst.Iif (Inst.Eq,falsel)];
       is;
       [Inst.Ilabel falsel;]]
  | RAst.IfThenElse (e,s1,s2) ->
    let falsel = Inst.make_label "false" in
    let endifl = Inst.make_label "endif" in
    let ie  = codegen_exp info e in
    let is1 = codegen_stm info s1 in
    let is2 = codegen_stm info s2 in
    List.concat 
      [ie;
       [Inst.Iif (Inst.Eq,falsel)];
       is1;
       [Inst.Igoto endifl;
        Inst.Ilabel falsel];
       is2;
       [Inst.Ilabel endifl;]]
  | RAst.While (e,s) ->
    let loopl = Inst.make_label "loop" in
    let condl = Inst.make_label "cond" in
    let ie = codegen_exp info e in
    let is = codegen_stm info s in
    List.concat
      [[Inst.Igoto condl;
        Inst.Ilabel loopl];
       is;
       [Inst.Ilabel condl];
       ie;
       [Inst.Iif (Inst.Ne,loopl)]]
  | RAst.Empty -> []
  | RAst.Block b ->
    codegen_stm_list info b


and codegen_stm_list info stms = Utils.concat_list (codegen_stm info) stms
 

let codegen_return_stm info rstm = match rstm.RAst.return_stm with
  | RAst.VoidReturn -> [Inst.Ireturn]
  | RAst.ValueReturn e ->
    let ie = codegen_exp info e in
    (match e.RAst.exp_type with
      | Types.Int
      | Types.Boolean -> 
        List.concat [ie;
                     [Inst.Iireturn]]
      | Types.String
      | Types.Class _
      | Types.Null ->
        List.concat [ie;
                     [Inst.Iareturn]]
      | Types.Void ->
        raise (Error.InternalCompilerError "Illegal type of return expression"))


let codegen_local info ldecl =
  let (typ,id,exp,offset) = ldecl in
  let is = codegen_exp info exp in
  match typ with
    | Types.Int
    | Types.Boolean -> List.concat [is; [Inst.Iistore offset]]  (* integer store *)
    | Types.Class _
    | Types.String -> List.concat [is; [Inst.Iastore offset]]  (* address store *)
    | Types.Void
    | Types.Null ->
      raise (Error.InternalCompilerError "Illegal type of local initializer")



let codegen_field info fdecl =
  { CAst.field_type      = fdecl.RAst.field_type;
    CAst.field_name      = fdecl.RAst.field_name;
    CAst.field_signature = fdecl.RAst.field_signature; }


let codegen_body info fab =
  let local_init = Utils.concat_list (codegen_local info) fab.RAst.locals in
  let body       = codegen_stm_list info fab.RAst.statements in
  let return     = codegen_return_stm info fab.RAst.return in
  { CAst.formals = fab.RAst.formals;
    CAst.body    = List.concat [local_init; body; return] }


let codegen_constructor info cdecl =
  let fab = codegen_body info cdecl.RAst.constructor_body in
  let supercall = [Inst.Iaload 0;
                   Inst.Iinvokespecial 
                     (codegen_method_sig_known "java/lang/Object/<init>()V" 0 0)
                  ]
  in
  let field_init =
      List.fold_right (fun fdecl flist ->
        match fdecl.RAst.field_init with
        | None -> flist
        | Some e -> 
          let ie = codegen_exp info e in
          let fieldsig = fdecl.RAst.field_signature
                   ^ " " ^ (Types.typeexp_to_sig fdecl.RAst.field_type) in
          List.concat [[Inst.Iaload 0];
                       ie;
                       [Inst.Iputfield fieldsig];
                       flist]
      ) info.nonstatic_fields []
  in
  let insts = List.concat [supercall; field_init; fab.CAst.body] in
  { CAst.constructor_name      = cdecl.RAst.constructor_name;
    CAst.constructor_body      = { fab with CAst.body = insts };
    CAst.constructor_signature = cdecl.RAst.constructor_signature }


let codegen_method info mdecl =
  { CAst.method_return_type = mdecl.RAst.method_return_type;
    CAst.method_name        = mdecl.RAst.method_name;
    CAst.method_body        = codegen_body info mdecl.RAst.method_body;
    CAst.method_signature   = mdecl.RAst.method_signature }


let codegen_program tenv prog =
  List.map (fun cdecl ->
    let class_name = cdecl.RAst.class_name in
    let class_type = 
      Env.lookup_env tenv "class" class_name.Ast.identifier class_name.Ast.identifier_pos in
    let info    = { tenv             = tenv;
                    class_type       = class_type;
                    nonstatic_fields = cdecl.RAst.class_fields } in
    let fields = List.map (codegen_field info) cdecl.RAst.class_fields  in
    let main   = Utils.opt (codegen_body info) cdecl.RAst.class_main in
    let mdecls = List.map (codegen_method info) cdecl.RAst.class_methods in
    { 
      CAst.source_file_name     = cdecl.RAst.source_file_name;
      CAst.class_name           = cdecl.RAst.class_name;
      CAst.class_fields         = fields;
      CAst.class_constructor    = codegen_constructor info cdecl.RAst.class_constructor;
      CAst.class_main           = main;
      CAst.class_methods        = mdecls;
      CAst.class_decl_signature = cdecl.RAst.class_decl_signature; }
  ) prog

