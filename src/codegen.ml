(** Compiler phase to generate Java bytecode for all
    method bodies.

    Code generation for a statement or local variable declaration
    leaves the stack height unchanged.

    Code generation for an expression pushes the result
    of the expression onto the stack if the type of
    the expression is non-void, or leaves the stack height
    unchanged if the type of the expression is void.
*)
open Types
type id = Ast.id

type prm = t * id * int (*NEW*)
(*type local   = t * id (* * exp *) * int (*NEW*)*)

type body =
    { prms    : prm list;
    (*locals  : local list; *)
      body    : Inst.instruction list; }

type field =
  | Method of t * id * body * string
  | Main of body
  | Constructor of id * body * string
  | Field of t * id (*field_init : exp option;*) * string

type class_decl =
  { cfilename : string;
    cname     : id;
    cfields   : field list;
    csig      : string (*NEW*) }

let mk_msig msig m_nargs m_nreturns =
  { Inst.msig; m_nargs; m_nreturns}

let f_msig id base m =
  let msig =
    let mname   = id.Ast.id in
    let basesig = cname_to_sig base in
    let prmsigs = List.map t_to_sig m.mprms_t in
    let ressig  = t_to_sig m.mresult_t in
    basesig ^ "/" ^ mname ^ "(" ^ (String.concat "" prmsigs) ^ ")" ^ ressig
  in
  mk_msig msig (List.length m.mprms_t) (if m.mresult_t = TVoid then 0 else 1)

let f_constructor_sig base (name,prms) =
    let msig =
      let basesig = cname_to_sig base in
      let prmsigs = List.map t_to_sig prms in
      basesig ^ "/<init>(" ^ (String.concat "" prmsigs) ^ ")V"
    in
    mk_msig msig (List.length prms) 0

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

type env = { tenv       : class_type M.t;
             class_type : class_type;
             fields     : Res.field list }

let rec f_lval_read env {Res.lval;lt} =
  match lval with
  | Res.Field (id, _) -> 
    let name = id.Ast.id in
    let jsig = t_to_sig lt in
    let csig = env.class_type.cname_t in
    [Inst.Iaload 0;
     Inst.Igetfield (csig ^ "/" ^ name ^ " " ^ jsig)]
  | Res.Local (id, i) -> 
    begin match lt with
    | TInt
    | TBool -> [Inst.Iiload i] (* integer load *)
    | TString 
    | TClass _ -> [Inst.Iaload i] (* address load *)
    | _ -> raise (Error.InternalCompilerError "Illegal type of lval")
    end

and f_lval_write env {Res.lval;lt} =
  match lval with
  | Res.Field (id,base) ->
    let name = id.Ast.id in
    let jsig = t_to_sig lt in
    let csig = env.class_type.cname_t in
    [Inst.Iaload(0);
     Inst.Iswap;
     Inst.Iputfield (csig ^ "/" ^ name ^ " " ^ jsig)]
  | Res.Local (id,i) -> 
    begin match lt with
    | TInt
    | TBool  -> [Inst.Iistore i] (* integer store *)
    | TString 
    | TClass _  -> [Inst.Iastore i] (* address store *)
    | _ -> raise (Error.InternalCompilerError "Illegal type of lval")
    end

and f_exp env {Res.exp;e_t} =
  match exp with
  | Res.Binop (e1, op, e2) ->
    f_exp env e1 @
    f_exp env e2 @
    begin match op with
    | Typing.Add -> [Inst.Iiadd] | Typing.Sub -> [Inst.Iisub]
    | Typing.Mul -> [Inst.Iimul] | Typing.Div -> [Inst.Iidiv]
    | Typing.Mod -> [Inst.Iirem] | Typing.And -> [Inst.Iiand]
    | Typing.Or  -> [Inst.Iior]  | Typing.Xor -> [Inst.Iixor]
    
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
    | Typing.Cat ->
      [ Inst.Iinvokevirtual (mk_msig 
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
      [Inst.Iinvokestatic (mk_msig msig 1 1)]
    | Typing.IntToString ->
      let msig = "java/lang/String/valueOf(I)Ljava/lang/String;" in
      [Inst.Iinvokestatic (mk_msig msig 1 1)]
    | Typing.CharToString ->
      let msig = "java/lang/String/valueOf(C)Ljava/lang/String;" in
      [Inst.Iinvokestatic (mk_msig msig 1 1)]
    | Typing.ObjectToString ->
      let msig = "java/lang/String/valueOf(Ljava/lang/Object;)Ljava/lang/String;" in
      [Inst.Iinvokestatic (mk_msig msig 1 1)]
    end
  | Res.IntConst i     -> [Inst.Ildc_int i]
  | Res.StringConst s  -> [Inst.Ildc_string s]
  | Res.BooleanConst b -> [Inst.Ildc_int (if b then 1l else 0l)]
  | Res.Null           -> [Inst.Iaconst_null]
  | Res.This           -> [Inst.Iaload 0]
  | Res.Invoke (e, id, es, mt) ->
    begin
      f_exp env e @
      List.concat (List.map (fun e -> f_exp env e) es) @
      Inst.Iinvokevirtual (f_msig id (show_t e.Res.e_t) mt) ::
      []
    end
  | Res.New (_, es, c) ->
    let typesig = show_t e_t in
    begin
      Inst.Inew typesig ::
      Inst.Idup ::
      List.concat (List.map (fun e -> f_exp env e) es) @
      Inst.Iinvokespecial (f_constructor_sig typesig c) ::
      []
    end
  | Res.Lvalue lval -> f_lval_read env lval
  | Res.Assignment (lval, e) ->
    begin
      f_exp env e @
      Inst.Idup ::
      f_lval_write env lval
    end
  | Res.Print e ->
    let prmt =
      match e.Res.e_t with
      | TInt -> "I"
      | TBool -> "Z"
      | TString
      | TClass _ -> "Ljava/lang/Object;"
      | _ -> raise (Error.InternalCompilerError "Illegal print type")
    in
    let msig = "java/io/PrintStream/print(" ^ prmt ^ ")V" in
    begin
      f_exp env e @
      Inst.Igetstatic "java/lang/System/out Ljava/io/PrintStream;" ::
      Inst.Iswap ::
      Inst.Iinvokevirtual (mk_msig msig 1 0) ::
      []
    end
  | Res.Read ->
    (* Generate code as if System.in.read() was called. *)
    [ Inst.Igetstatic "java/lang/System/in Ljava/io/InputStream;";
      Inst.Iinvokevirtual (mk_msig "java/io/InputStream/read()I" 0 1)
    ]

let rec f_stm env {Res.stm} =
  match stm with
  | Res.Exp e ->
    let ie = f_exp env e in
    begin match e.Res.e_t with
    | TVoid -> ie
    | _ -> ie @ [Inst.Ipop]
    end
  | Res.If (e,s, {Res.stm=Res.Empty}) ->
    let falsel = Inst.make_label "false" in
    begin
      f_exp env e @
      Inst.Iif (Inst.Eq,falsel) ::
      f_stm env s @
    Inst.Ilabel falsel ::
      []
    end
  | Res.If (e,s1,s2) ->
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
  | Res.Block ss -> List.concat (List.map (f_stm env) ss)

let f_rstm env {Res.rstm} =
  match rstm with
  | Res.VoidReturn -> [Inst.Ireturn]
  | Res.ValueReturn e ->
    f_exp env e @
    begin match e.Res.e_t with
    | TInt
    | TBool -> [Inst.Iireturn]
    | TString
    | TClass _
    | TNull -> [Inst.Iareturn]
    | TVoid -> raise (Error.InternalCompilerError "Illegal type of return expression")
    end

let f_local env (t, id, exp, offset) =
  match t with
  | TInt
  | TBool -> f_exp env exp @ [Inst.Iistore offset]  (* integer store *)
  | TClass _
  | TString -> f_exp env exp @ [Inst.Iastore offset]  (* address store *)
  | TVoid
  | TNull -> raise (Error.InternalCompilerError "Illegal type of local initializer")

let f_body env {Res.locals;stms;return;prms} =
  let locals = List.concat (List.map (f_local env) locals) in
  let stms   = List.concat (List.map (f_stm env) stms) in
  let return = f_rstm env return in
  { prms; body = locals @ stms @ return }

let f_field env = function
  | Res.Method(t, name, body, jsig) -> Method (t, name, f_body env  body,  jsig)
  | Res.Constructor(name, body, jsig) ->
    let field_init =
        List.fold_right (fun field insts ->
          begin match field with
          | Res.Field(t,_,init,jsig) ->
            begin match init with
            | None -> insts
            | Some e -> 
              let fsig = jsig ^ " " ^ (t_to_sig t) in
              begin
                Inst.Iaload 0 ::
                f_exp env e @
                Inst.Iputfield fsig ::
                insts
              end
            end
          | _ -> insts
          end
        ) env.fields []
    in
    let body = f_body env body in
    let insts = 
      Inst.Iaload 0 ::
      Inst.Iinvokespecial (mk_msig "java/lang/Object/<init>()V" 0 0) ::
      field_init @ body.body in
    Constructor (name, { body with body = insts }, jsig)
  | Res.Main body -> Main (f_body env body)
  | Res.Field (t, name, _, jsig) -> Field(t, name, jsig)

let f tenv prog =
  List.map (fun {Res.cname;cfields;csig;cfilename} ->
    let class_type = Env.lookup_env tenv "class" cname.Ast.id cname.Ast.id_pos in
    let env = { tenv; class_type; fields = cfields } in
    { 
      cfilename;
      cname;
      cfields = List.map (f_field env) cfields;
      csig = csig;
    }
  ) prog

