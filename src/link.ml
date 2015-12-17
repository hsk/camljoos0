(** Compiler phase to check the uses of all types and variables
    to their definitions.
*)

open Types

type id = Ast.id
type binop = Ast.binop
type unop = Ast.binop
type lval = Ast.lval
type exp  = Ast.exp
type stm  = Ast.stm
type rstm = Ast.rstm

type prm   = t * id
type local = t * id * exp

type body =
  { prms   : prm list;
    throws : string list;
    locals : local list;
    stms   : stm list;
    return : rstm;
  }

type field = 
  | Method of t * id * body
  | Constructor of id * body
  | Main of body
  | Field of t * id * exp option

type class_decl =
  { 
    cfilename : string;
    cname     : id;
    cfields   : field list;
  }

let f_t tenv {Ast.t} =
  match t with
  | Ast.TVoid     -> TVoid
  | Ast.TInt      -> TInt
  | Ast.TBoolean  -> TBool
  | Ast.TString   -> TString
  | Ast.TClass {Ast.id; id_pos} ->
    if not (M.mem id tenv) then Error.error id_pos "The class '%s' is not defined." id;
    TClass id

let f_lval (tenv, {cfield_ts}, lenv) {Ast.lval} =
  match lval with
  | Ast.Local {Ast.id; id_pos} ->
    if not (List.mem id lenv) then Error.error id_pos "The variable '%s' is not defined." id
  | Ast.Field {Ast.id; id_pos} ->
    if not (List.exists (fun {ft_name} -> ft_name = id) cfield_ts) then
      Error.error id_pos "The field '%s' is not defined." id

let rec f_exp ((tenv,_,_) as env) {Ast.exp} =
  match exp with
  | Ast.Binop (l, _, r) -> f_exp env l; f_exp env r
  | Ast.Unop (_, e) -> f_exp env e 
  | Ast.IntConst _ | Ast.StringConst _ | Ast.BooleanConst _
  | Ast.Null | Ast.This -> ()
  | Ast.Invoke (e, _, es) -> f_exp env e; List.iter (f_exp env) es
  | Ast.New ({Ast.id;id_pos}, es) ->
      if not (M.mem id tenv) then Error.error id_pos "The class '%s' is not defined." id;
      List.iter (f_exp env) es
  | Ast.Lvalue lval -> f_lval env lval
  | Ast.Assignment (lval, e) -> f_lval env lval; f_exp env e
  | Ast.Print e -> f_exp env e
  | Ast.Read -> ()

let rec f_stm env {Ast.stm} =
  match stm with
  | Ast.Exp e -> f_exp env e
  | Ast.If (e, s1, s2) -> f_exp env e; f_stm env s1; f_stm env s2
  | Ast.While (e, s) -> f_exp env e; f_stm env s
  | Ast.Empty -> ()
  | Ast.Block ss -> List.iter (f_stm env) ss

let f_rstm env {Ast.rstm} =
  match rstm with
  | Ast.VoidReturn    -> ()
  | Ast.ValueReturn e -> f_exp env e

let f_body tenv ct {Ast.prms;throws;locals;stms;return} =
  let (lenv, prms) = List.fold_left(fun (lenv, prms) (t, id) ->
    (id.Ast.id::lenv, (f_t tenv t, id)::prms)
  ) ([], []) prms in
  let (lenv, locals) = List.fold_left(fun (lenv, locals) (t, id, e) ->
    f_exp (tenv, ct, lenv) e;
    (id.Ast.id::lenv, (f_t tenv t, id, e)::locals)
  ) (lenv, []) locals in
  List.iter (f_stm (tenv, ct, lenv)) stms;
  f_rstm (tenv, ct, lenv) return;
  {prms = List.rev prms; throws; locals = List.rev locals; stms; return}

let f_field tenv ct = function
  | Ast.Field(t, id, init) ->
    ignore (Types.opt (f_exp (tenv, ct, [])) init); (*no locals*)
    Field(f_t tenv t, id, init)
  | Ast.Method(t, id, body) -> Method(f_t tenv t, id, f_body tenv ct body)
  | Ast.Constructor(id, body) -> Constructor(id, f_body tenv ct body)
  | Ast.Main(body) -> Main(f_body tenv ct body)

let rec f tenv prog =
  List.map (fun {Ast.cfilename;cname;cfields} ->
    { cfilename; cname;
      cfields = List.map (f_field tenv (M.find (cname.Ast.id) tenv)) cfields;
    }
  ) prog

