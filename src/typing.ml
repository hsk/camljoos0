(** Compiler phase to perform type checking of the program.
    It also performs type coercions and other transformations
    based on the types.
*)
open Types

type id = Ast.id

type binop =
  | Add | Sub
  | Mul | Div
  | Mod
  | Eq | Ne
  | Lt | Le
  | Gt | Ge
  | And | Or | Xor
  | Aeq  (*NEW*) | Ane  (*NEW*)
  | Cat (* Only created for + (char) *)

type unop =
  | Negate     (*minus*)
  | Complement (*complement*)
  | BooleanToString  (*NEW*)
  | IntToString      (*NEW*)
  | CharToString
  | ObjectToString   (*NEW*)

type lval = { lpos: Lexing.position; lt : t; lval: lval_desc }
and lval_desc =
  | Local of id
  | Field of id * ft (*NEW*)

type exp = { e_pos: Lexing.position; e_t : t; exp: exp_desc }
and exp_desc =
  | Binop of exp * binop * exp
  | Unop of unop * exp
  | IntConst of int32
  | StringConst of string
  | BooleanConst of bool
  | Null
  | This
  | Invoke of exp * id * exp list * mt (*NEW*)
  | New of id * exp list * constructor_t (*NEW*)
  | Lvalue of lval
  | Assignment of lval * exp
  | Print of exp
  | Read

type stm = { stm_pos: Lexing.position; stm: stm_desc }
and stm_desc =
  | Exp of exp
  | If of exp * stm * stm
  | While of exp * stm
  | Empty
  | Block of stm list

type rstm = { rstm_pos: Lexing.position; rstm: rstm_desc }
and rstm_desc = 
  | VoidReturn
  | ValueReturn of exp

type prm   = t * id
type local = t * id * exp
type body =
    { prms    : prm list;
      throws  : string list;
      locals  : local list;
      stms    : stm list;
      return  : rstm; }

type field = 
  | Constructor of id * body
  | Method of t * id * body
  | Main of body
  | Field of t * id * exp option

type class_decl =
    { cfilename  : string;
      cname      : id;
      cfields    : field list; }

let error = Error.error

let rec show_t = function
  | TVoid       -> "void"
  | TInt        -> "int"
  | TBool       -> "boolean"
  | TString     -> "String"
  | TClass name -> name
  | TNull       -> "null"

let show_binop = function
  | Add -> "plus"
  | Sub -> "minus"
  | Mul -> "times"
  | Div -> "divide"
  | Mod -> "modulo"
  | Eq  -> "eq"
  | Ne  -> "ne"
  | Lt  -> "lt"
  | Le  -> "le"
  | Gt  -> "gt"
  | Ge  -> "ge"
  | And -> "and"
  | Or  -> "or"
  | Xor -> "xor"
  | Cat -> "+"
  | Aeq  -> "eq"
  | Ane  -> "ne"

let show_unop = function
  | Ast.Negate       -> "negate"
  | Ast.Complement   -> "complement"
  | Ast.CharToString -> "+(char)"

let rec lookup_method id pos meths = 
  begin try
    List.find (fun {mname_t} -> mname_t = id) meths
  with _ -> error pos "The method '%s' is not defined." id
  end

let bin_error lexp binop rexp pos =
  error pos "The operator %s cannot be used with the types %s and %s"
    (show_binop binop) (show_t lexp.e_t) (show_t rexp.e_t)

let un_error unop exp pos =
  error pos "The operator %s cannot be used with the type %s"
    (show_unop unop) (show_t exp.e_t)

let assign_error to_t from_t pos =
  error pos "The type %s cannot be assigned to the type "
    (show_t from_t) (show_t to_t)

let f_assign to_t from_t pos =
  match (from_t, to_t) with
  | (from_t, to_t) when to_t = from_t -> ()
  | (TNull, TClass _)
  | (TNull, TString) -> ()
  | _ -> assign_error to_t from_t pos

let f_lval tenv ct lenv {Ast.lpos;lval} = 
  let mk (lval, lt) = { lpos; lt; lval } in
  mk begin match lval with
  | Ast.Local ({Ast.id;id_pos} as l) ->
    (Local l, Env.lookup_env lenv "variable" id id_pos)
  | Ast.Field ({Ast.id} as f)  ->
    let ft = List.find (fun {ft_name} -> ft_name = id) ct.cfield_ts in
    (Field (f, ft), ft.ft)
  end

let rec f_prms to_ts from_ts kind pos =
  begin try
    List.iter2 (fun to_t from_t ->
      f_assign to_t from_t pos
    ) to_ts from_ts
  with
    | _ -> error pos "Incorrect number of arguments for %s" kind
  end

let rec f_exp tenv ct lenv {Ast.e_pos;exp} = 
  let mk (exp,e_t) = { e_pos; e_t; exp } in
  mk begin match exp with
  | Ast.Binop (l, op, r) ->
    let l = f_exp tenv ct lenv l in
    let r = f_exp tenv ct lenv r in
    let op = match op with
      | Ast.Add -> Add | Ast.Sub -> Sub
      | Ast.Mul -> Mul | Ast.Div -> Div | Ast.Mod -> Mod
      | Ast.Eq  -> Eq  | Ast.Ne  -> Ne
      | Ast.Lt  -> Lt  | Ast.Le  -> Le
      | Ast.Gt  -> Gt  | Ast.Ge  -> Ge
      | Ast.And -> And | Ast.Or  -> Or
      | Ast.Xor -> Xor | Ast.Cat -> Cat
    in
    let err () = bin_error l op r e_pos in
    begin match (l.e_t, op, r.e_t) with
    | TInt,  Add, TInt
    | TInt,  Sub, TInt
    | TInt,  Mul, TInt
    | TInt,  Div, TInt
    | TInt,  Mod, TInt -> (Binop (l, op, r), l.e_t)

    | TInt,  Eq,  TInt
    | TInt,  Ne,  TInt
    | TInt,  Lt,  TInt
    | TInt,  Le,  TInt
    | TInt,  Gt,  TInt
    | TInt,  Ge,  TInt -> (Binop (l, op, r), TBool)

    | TBool, Eq,  TBool
    | TBool, Ne,  TBool
    | TBool, And, TBool
    | TBool, Or,  TBool
    | TBool, Xor, TBool -> (Binop (l, op, r), TBool)

    | TNull, Eq,  TNull    -> (Binop (l, Aeq, r), TBool)
    | TNull, Ne,  TNull    -> (Binop (l, Ane, r), TBool)
    | TNull, Eq,  TString  -> (Binop (l, Aeq, r), TBool)
    | TNull, Ne,  TString  -> (Binop (l, Ane, r), TBool)
    | TNull, Eq,  TClass _ -> (Binop (l, Aeq, r), TBool)
    | TNull, Ne,  TClass _ -> (Binop (l, Ane, r), TBool)

    | TString,  Eq, TNull    -> (Binop (l, Aeq, r), TBool)
    | TString,  Ne, TNull    -> (Binop (l, Ane, r), TBool)
    | TString,  Eq, TString  -> (Binop (l, Aeq, r), TBool)
    | TString,  Ne, TString  -> (Binop (l, Ane, r), TBool)

    | TClass _, Eq, TNull    -> (Binop (l, Aeq, r), TBool)
    | TClass _, Ne, TNull    -> (Binop (l, Ane, r), TBool)
    | TClass a, Eq, TClass b -> if a <> b then err (); (Binop (l, Aeq, r), TBool)
    | TClass a, Ne, TClass b -> if a <> b then err (); (Binop (l, Ane, r), TBool)

    (* Only occurs with + (char) exp
       Both arguments must be of string type *)
    | TString,  Cat, TString  -> (Binop (l, op, r), TString)
    (* String concatenation *)
    | TString,  Add, _      
    | _,        Add, TString  ->
      (* Insert a conversion to string of the given expression. *)
      let str e =
        let mk uop = { e with exp = Unop (uop, e) } in
        match e.e_t with
        | TVoid   -> err ()
        | TInt    -> mk IntToString
        | TBool   -> mk BooleanToString
        | TClass _
        | TNull   -> mk ObjectToString
          (* Null values are not subject to constant folding, so
             just replacing by the string "null" could lead to
             incorrect results. *)
        | TString -> 
           (* If the expression is not a constant or the result
              of a string concatenation or conversion, it may
              be null. Since null values must be coerced to the
              string "null", we must insert a conversion to string
              even though the value is already a string. *)
          begin match e.exp with
          | StringConst _
          | Binop _
          | Unop _ -> e
          | _      -> mk ObjectToString
          end
      in
      (Binop (str l, Cat, str r), TString)
    | _ -> err ()
    end

  | Ast.Unop (op, e) ->
    let e = f_exp tenv ct lenv e in
    begin match (op, e.e_t) with
      | (Ast.Negate,      TInt)  -> (Unop (Negate,       e), TInt)
      | (Ast.Complement,  TBool) -> (Unop (Complement,   e), TBool)
      | (Ast.CharToString,TInt)  -> (Unop (CharToString, e), TString)
      | _ -> un_error op e e_pos
    end
  | Ast.IntConst     i -> (IntConst     i, TInt)
  | Ast.StringConst  s -> (StringConst  s, TString)
  | Ast.BooleanConst b -> (BooleanConst b, TBool)
  | Ast.Null           -> (Null          , TNull)
  | Ast.This           -> (This          , TClass ct.cname_t)
  | Ast.Invoke (e, ({Ast.id_pos} as m), es) ->
    let e  = f_exp tenv ct lenv e in
    let es = List.map (f_exp tenv ct lenv) es in
    begin match e.e_t with
    | TClass cid ->
      let ct = M.find cid tenv in
      let mt = lookup_method m.Ast.id id_pos ct.cmethod_ts in
      let ts = List.map (fun {e_t} -> e_t) es in
      f_prms mt.mprms_t ts "method" id_pos;
      (Invoke (e, m, es, mt), mt.mresult_t)
    | _ -> error e.e_pos "Only class types can be method invocation receivers"
    end
  | Ast.New (id, es) ->
    let es = List.map (f_exp tenv ct lenv) es in
    let ct = M.find id.Ast.id tenv in
    let (name, prms) as constructor = ct.cconstruct_t in
    let ts = List.map (fun {e_t} -> e_t) es in
    f_prms prms ts "constructor" id.Ast.id_pos;
    (New (id, es, constructor), TClass id.Ast.id)
    
  | Ast.Lvalue lval ->
    let lval = f_lval tenv ct lenv lval in
    (Lvalue lval, lval.lt)
  | Ast.Assignment (lval, e) ->
    let lval = f_lval tenv ct lenv lval in
    let e    = f_exp tenv ct lenv e in
    f_assign lval.lt e.e_t e.e_pos;
    (Assignment (lval, e), lval.lt)
  | Ast.Print e ->
    let {e_t;e_pos} as e = f_exp tenv ct lenv e in
    if e_t = TVoid then error e_pos "Value to be printed must not be void";
    (Print e, TVoid)
  | Ast.Read -> (Read, TInt)
  end

let rec f_stm tenv ct lenv {Ast.stm_pos;stm} = 
  match stm with
  | Ast.Exp e -> { stm_pos; stm = Exp (f_exp tenv ct lenv e)}
  | Ast.If (e, s1, s2) ->
    let e = f_exp tenv ct lenv e in
    if e.e_t <> TBool then error e.e_pos "Condition must have type boolean";
    let s1 = f_stm tenv ct lenv s1 in
    { stm_pos; stm = If (e, s1, f_stm tenv ct lenv s2) }
  | Ast.While (e, s) ->
    let ({e_t;e_pos} as e) = f_exp tenv ct lenv e in
    if e_t <> TBool then error e_pos "Condition must have type boolean";
    { stm_pos; stm = While (e, f_stm tenv ct lenv s) }
  | Ast.Empty -> { stm_pos; stm = Empty } 
  | Ast.Block stms -> { stm_pos; stm = Block (List.map(f_stm tenv ct lenv) stms) }

let f_rstm tenv ct lenv rt {Ast.rstm_pos;rstm} =
  match rstm with
  | Ast.VoidReturn ->
    if rt <> TVoid then error rstm_pos "A non-void method must return a value";
    { rstm_pos; rstm = VoidReturn }
  | Ast.ValueReturn e ->
    let ({e_t} as e) = f_exp tenv ct lenv e in
    f_assign rt e_t rstm_pos;
    { rstm_pos; rstm = ValueReturn e }

let f_body tenv ct rt {Link.prms;throws;locals;stms;return} = 
  let (prms, lenv) = List.fold_left (fun (prms, lenv) ((t, id) as prm) ->
    (prm::prms, M.add id.Ast.id t lenv)
  ) ([], M.empty) prms in
  let (locals, lenv) = List.fold_left (fun (locals, lenv) (t, id, e) ->
    let ({e_t;e_pos} as e) = f_exp tenv ct lenv e in
    f_assign t e_t e_pos;
    ((t, id, e)::locals, M.add id.Ast.id t lenv)
  ) ([], lenv) locals in
  { prms = List.rev prms;
    throws;
    locals  = List.rev locals;
    stms    = List.map (f_stm tenv ct lenv) stms;
    return  = f_rstm tenv ct lenv rt return;
  }

let f_field tenv ({cname_t} as ct) = function
  | Link.Method (t, name, body) -> Method(t, name, f_body tenv ct t body)
  | Link.Constructor ({Ast.id; id_pos} as name, body) ->
    if id <> cname_t then error id_pos "Constructor must have the same name as its enclosing class";
    Constructor(name, f_body tenv ct TVoid body)
  | Link.Main(body) -> Main(f_body tenv ct TVoid body)
  | Link.Field(t, name, init) ->
    let init = Types.opt(f_exp tenv ct M.empty) init in
    begin match init with
      | None -> ()
      | Some {e_t; e_pos} -> f_assign t e_t e_pos
    end;
    Field(t, name, init)

let rec f tenv prog =
  List.map (fun {Link.cfilename; cname; cfields} ->
    let {Ast.id; id_pos} = cname in
    let jname = id ^ ".java" in
    if jname <> Filename.basename cfilename then
      error id_pos "The public class '%s' must be declared in a file called %s" id jname;
    let t = Env.lookup_env tenv "class" id id_pos in
    { cfilename; cname; cfields = List.map (f_field tenv t) cfields; }
  ) prog

