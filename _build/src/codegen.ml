(** Compiler phase to generate Java bytecode for all
    method bodies.

    Code generation for a statement or local variable declaration
    leaves the stack height unchanged.

    Code generation for an expression pushes the result
    of the expression onto the stack if the type of
    the expression is non-void, or leaves the stack height
    unchanged if the type of the expression is void.
*)

type identifier = Ast.identifier


type typeexp = Types.typeexp


type formal_param = typeexp * identifier * int (*NEW*)
(*type local_decl   = typeexp * identifier (* * exp *) * int (*NEW*)*)

type field_decl = typeexp * identifier (*field_init : exp option;*) * string

type body =
    { formals    : formal_param list;
(*    locals     : local_decl list; *)
      body       : Inst.instruction list; }

type method_decl =
  | Method of typeexp * identifier * body * string
  | Main of body
  | Constructor of identifier * body * string

type class_decl =
    { source_file_name     : string;
      class_name           : identifier;
      class_fields         : field_decl list;
      class_methods        : method_decl list;
      class_decl_signature : string (*NEW*) }

type program = class_decl list

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

let codegen_constructor_sig base (name,formals) =
    let basesig = Types.cname_to_sig base in
    let argsigs = List.map Types.typeexp_to_sig formals in
    let numargs = List.length formals in
    let msig    = basesig ^ "/<init>(" ^ (String.concat "" argsigs) ^ ")V" in
    codegen_method_sig_known msig numargs 0

let codegen_cond = function
  | Typing.Eq -> Inst.Eq
  | Typing.Ne -> Inst.Ne
  | Typing.Lt -> Inst.Lt
  | Typing.Le -> Inst.Le
  | Typing.Gt -> Inst.Gt
  | Typing.Ge -> Inst.Ge
  | Typing.Aeq -> Inst.Aeq
  | Typing.Ane -> Inst.Ane
  | _ -> raise (Error.InternalCompilerError "Illegal cond in binary operation")


type info = { tenv             : Types.class_type Types.M.t;
              class_type       : Types.class_type;
              nonstatic_fields : Res.field_decl list }


let rec codegen_lvalue_read info {Res.lvalue;lvalue_type} =
  match lvalue with
  | Res.Field (id, ftype) -> 
    let fieldname = id.Ast.identifier in
    let fieldtype = Types.typeexp_to_sig lvalue_type in
    let basesig   = info.class_type.Types.class_name in
    [Inst.Iaload 0;
     Inst.Igetfield (basesig ^ "/" ^ fieldname ^ " " ^ fieldtype)]
  | Res.Local (id, i) -> 
    (match lvalue_type with
      | Types.Int
      | Types.Boolean -> [Inst.Iiload i] (* integer load *)
      | Types.String 
      | Types.Class _ -> [Inst.Iaload i] (* address load *)
      | _ -> raise (Error.InternalCompilerError "Illegal type of lvalue") )


and codegen_lvalue_write info {Res.lvalue;lvalue_type} =
  match lvalue with
    | Res.Field (id,base) ->
      let fieldname = id.Ast.identifier in
      let fieldtype = Types.typeexp_to_sig lvalue_type in
      let basesig   = info.class_type.Types.class_name in
      [Inst.Iaload(0);
       Inst.Iswap;
       Inst.Iputfield (basesig ^ "/" ^ fieldname ^ " " ^ fieldtype)]
    | Res.Local (id,i) -> 
      (match lvalue_type with
        | Types.Int
        | Types.Boolean  -> [Inst.Iistore i] (* integer store *)
        | Types.String 
        | Types.Class _  -> [Inst.Iastore i] (* address store *)
        | _ -> raise (Error.InternalCompilerError "Illegal type of lvalue") )


and codegen_exp info {Res.exp;exp_type} =
  match exp with
  | Res.Binop (e1,op,e2) ->
    let ie1 = codegen_exp info e1 in
    let ie2 = codegen_exp info e2 in
    (match op with
      | Typing.Add    -> List.concat [ie1; ie2; [Inst.Iiadd]]
      | Typing.Minus  -> List.concat [ie1; ie2; [Inst.Iisub]]
      | Typing.Times  -> List.concat [ie1; ie2; [Inst.Iimul]]
      | Typing.Divide -> List.concat [ie1; ie2; [Inst.Iidiv]]
      | Typing.Modulo -> List.concat [ie1; ie2; [Inst.Iirem]]
      | Typing.And    -> List.concat [ie1; ie2; [Inst.Iiand]]
      | Typing.Or     -> List.concat [ie1; ie2; [Inst.Iior]]
      | Typing.Xor    -> List.concat [ie1; ie2; [Inst.Iixor]]
      | Typing.Eq    
      | Typing.Ne    
      | Typing.Lt
      | Typing.Le
      | Typing.Gt
      | Typing.Ge
      | Typing.Aeq
      | Typing.Ane    ->
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
      | Typing.Concat ->
        List.concat [ie1;
                     ie2;
                     [Inst.Iinvokevirtual
                         (codegen_method_sig_known 
                            "java/lang/String/concat(Ljava/lang/String;)Ljava/lang/String;" 1 1)]]
    )
  | Res.Unop (op,e) ->
    let ie = codegen_exp info e in
    (match op with
      | Typing.Negate     -> List.concat [ie; [Inst.Iineg]]
      | Typing.Complement ->
          let truel = Inst.make_label "true" in
          let endl = Inst.make_label "end" in
          List.concat [ie; [Inst.Iif(Inst.Eq,truel);
                            Inst.Ildc_int 0l;
                            Inst.Igoto endl;
                            Inst.Ilabel truel;
                            Inst.Ildc_int 1l;
                            Inst.Ilabel endl]]
      | Typing.BooleanToString ->
        let msig = "java/lang/String/valueOf(Z)Ljava/lang/String;" in
        List.concat [ie; 
                     [Inst.Iinvokestatic (codegen_method_sig_known msig 1 1) ]]
      | Typing.IntToString ->
        let msig = "java/lang/String/valueOf(I)Ljava/lang/String;" in
        List.concat [ie; 
                     [Inst.Iinvokestatic (codegen_method_sig_known msig 1 1) ]]
      | Typing.CharToString ->
        let msig = "java/lang/String/valueOf(C)Ljava/lang/String;" in
        List.concat [ie; 
                     [Inst.Iinvokestatic (codegen_method_sig_known msig 1 1) ]]
      | Typing.ObjectToString ->
        let msig = 
          "java/lang/String/valueOf(Ljava/lang/Object;)Ljava/lang/String;" in
        List.concat [ie; 
                     [Inst.Iinvokestatic (codegen_method_sig_known msig 1 1) ]]
    )
  | Res.IntConst i     -> [Inst.Ildc_int i]
  | Res.StringConst s  -> [Inst.Ildc_string s]
  | Res.BooleanConst b -> [Inst.Ildc_int (if b then 1l else 0l)]
  | Res.Null           -> [Inst.Iaconst_null]
  | Res.This           -> [Inst.Iaload 0]
  | Res.Invoke (e,id,es,mtype) ->
    let ie = codegen_exp info e in
    let ies = List.concat (List.map (fun e -> codegen_exp info e) es) in
    let recvtype = e.Res.exp_type in
    let msig = (codegen_method_sig id (Types.typeexp_to_string recvtype) mtype) in
    let invokeinst = [Inst.Iinvokevirtual msig] in
    List.concat [ie;
                 ies;
                 invokeinst]
  | Res.New (typ,es,c) ->
    let ies = List.concat (List.map (fun e -> codegen_exp info e) es) in
    let typesig = Types.typeexp_to_string exp_type in
    List.concat [[Inst.Inew typesig;
                  Inst.Idup];
                 ies;
                 [Inst.Iinvokespecial (codegen_constructor_sig typesig c)]]
  | Res.Lvalue lval ->
    codegen_lvalue_read info lval
  | Res.Assignment (lval,e) ->
    let ie = codegen_exp info e in
    let writeinst = codegen_lvalue_write info lval in
    List.concat [ie;
                 [Inst.Idup];
                 writeinst]
  | Res.Print e ->
    let ie      = codegen_exp info e in
    let argtype = (match e.Res.exp_type with
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
  | Res.Read ->
    (* Generate code as if System.in.read() was called. *)
    [Inst.Igetstatic "java/lang/System/in Ljava/io/InputStream;";
     Inst.Iinvokevirtual (codegen_method_sig_known "java/io/InputStream/read()I" 0 1)]


let rec codegen_stm info {Res.stm} =
  match stm with
  | Res.Exp e ->
    let ie = codegen_exp info e in
    (match e.Res.exp_type with
      | Types.Void -> ie
      | _ -> ie @ [Inst.Ipop] )
  | Res.IfThen (e,s) ->
    let falsel = Inst.make_label "false" in
    let ie = codegen_exp info e in
    let is = codegen_stm info s in
    List.concat
      [ie;
       [Inst.Iif (Inst.Eq,falsel)];
       is;
       [Inst.Ilabel falsel]]
  | Res.IfThenElse (e,s1,s2) ->
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
       [Inst.Ilabel endifl]]
  | Res.While (e,s) ->
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
  | Res.Empty -> []
  | Res.Block b ->
    codegen_stm_list info b


and codegen_stm_list info stms = Utils.concat_list (codegen_stm info) stms
 

let codegen_return_stm info {Res.return_stm} =
  match return_stm with
  | Res.VoidReturn -> [Inst.Ireturn]
  | Res.ValueReturn e ->
    let ie = codegen_exp info e in
    (match e.Res.exp_type with
      | Types.Int
      | Types.Boolean -> 
        List.concat [ie; [Inst.Iireturn]]
      | Types.String
      | Types.Class _
      | Types.Null ->
        List.concat [ie; [Inst.Iareturn]]
      | Types.Void ->
        raise (Error.InternalCompilerError "Illegal type of return expression")
    )


let codegen_local info (typ, id, exp, offset) =
  match typ with
  | Types.Int
  | Types.Boolean -> List.concat [codegen_exp info exp; [Inst.Iistore offset]]  (* integer store *)
  | Types.Class _
  | Types.String -> List.concat [codegen_exp info exp; [Inst.Iastore offset]]  (* address store *)
  | Types.Void
  | Types.Null -> raise (Error.InternalCompilerError "Illegal type of local initializer")


let codegen_field info {Res.field_type;field_name;field_signature} =
  (field_type, field_name, field_signature)


let codegen_body info {Res.locals;statements;return;formals} =
  let local_init = Utils.concat_list (codegen_local info) locals in
  let body       = codegen_stm_list info statements in
  let return     = codegen_return_stm info return in
  { formals = formals;
    body    = List.concat [local_init; body; return] }


let codegen_method info = function
  | Res.Method(return_type, name, body, signature) ->
    Method (return_type, name, codegen_body info  body,  signature)
  | Res.Constructor(name, body, signature) ->
    let fab = codegen_body info body in
    let supercall =
      [Inst.Iaload 0; Inst.Iinvokespecial (codegen_method_sig_known "java/lang/Object/<init>()V" 0 0)]
    in
    let field_init =
        List.fold_right (fun fdecl flist ->
          match fdecl.Res.field_init with
          | None -> flist
          | Some e -> 
            let ie = codegen_exp info e in
            let fieldsig = fdecl.Res.field_signature
                     ^ " " ^ (Types.typeexp_to_sig fdecl.Res.field_type) in
            List.concat [[Inst.Iaload 0];
                         ie;
                         [Inst.Iputfield fieldsig];
                         flist]
        ) info.nonstatic_fields []
    in
    let insts = List.concat [supercall; field_init; fab.body] in
    Constructor (name, { fab with body = insts }, signature)
  | Res.Main (body) -> Main (codegen_body info body)

let codegen_program tenv prog =
  List.map (fun {Res.class_name;class_fields;class_methods;class_decl_signature;source_file_name} ->
    let class_type = 
      Env.lookup_env tenv "class" class_name.Ast.identifier class_name.Ast.identifier_pos in
    let info    = { tenv             = tenv;
                    class_type       = class_type;
                    nonstatic_fields = class_fields } in
    { 
      source_file_name     = source_file_name;
      class_name           = class_name;
      class_fields         = List.map (codegen_field info) class_fields;
      class_methods        = List.map (codegen_method info) class_methods;
      class_decl_signature = class_decl_signature; }
  ) prog

