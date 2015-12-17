(** Compiler phase to calculate resource information,
    such as JVM signatures and local variable indices.
*)
open Types

type id = Ast.id
type binop = Typing.binop
type unop = Typing.unop

type lval = { lpos: Lexing.position; lt : t; lval: lval_desc }
and lval_desc =
  | Local of id * int (*NEW*)
  | Field of id * ft

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

type prm = t * id * int (*NEW*)
type local   = t * id * exp * int (*NEW*)

type body =
    { prms   : prm list;
      throws : string list;
      locals : local list;
      stms   : stm list;
      return : rstm; }

type field =
  | Constructor of id * body * string(*NEW*)
  | Method of t * id * body * string(*NEW*)
  | Main of body
  | Field of t * id * exp option * string(*NEW*)

type class_decl =
    { cfilename : string;
      cname     : id;
      cfields   : field list;
      csig      : string (*NEW*) }

module LocalsEnv = Map.Make(String)


let rec f_lval lenv {Typing.lpos;lt;lval} = 
  let mk lval = { lpos; lval; lt } in
  mk begin match lval with
  | Typing.Local id -> Local (id, LocalsEnv.find id.Ast.id lenv)
  | Typing.Field (id, ty) -> Field (id, ty)
  end

and f_exp lenv {Typing.e_pos;e_t;exp} =
  let mk exp = { e_pos; exp; e_t } in
  mk begin match exp with
  | Typing.Binop (e1, op, e2) -> Binop (f_exp lenv e1, op, f_exp lenv e2)
  | Typing.Unop (op, e)   -> Unop (op, f_exp lenv e)
  | Typing.IntConst i     -> IntConst i
  | Typing.StringConst s  -> StringConst s
  | Typing.BooleanConst b -> BooleanConst b
  | Typing.Null           -> Null
  | Typing.This           -> This
  | Typing.Invoke (e, id, es, mt) -> Invoke (f_exp lenv e, id, List.map (f_exp lenv) es, mt)
  | Typing.New (t, es, ct) -> New (t, List.map (f_exp lenv) es, ct)
  | Typing.Lvalue lv -> Lvalue (f_lval lenv lv)
  | Typing.Assignment (lv, e) -> Assignment (f_lval lenv lv, f_exp lenv e)
  | Typing.Print e -> Print (f_exp lenv e)
  | Typing.Read -> Read
  end

let rec f_stm lenv {Typing.stm_pos;stm} = 
  let mk stm = { stm_pos; stm } in
  mk begin match stm with
  | Typing.Exp e -> Exp (f_exp lenv e)
  | Typing.If (e, s1, s2) -> If (f_exp lenv e, f_stm lenv s1, f_stm lenv s2)
  | Typing.While (e, s) -> While (f_exp lenv e, f_stm lenv s)
  | Typing.Empty -> Empty
  | Typing.Block ss -> Block (List.map (f_stm lenv) ss)
  end

let f_rstm lenv {Typing.rstm_pos=pos;rstm} = 
  let mk s = { rstm_pos = pos; rstm = s } in
  mk begin match rstm with
  | Typing.VoidReturn    -> VoidReturn
  | Typing.ValueReturn e -> ValueReturn (f_exp lenv e)
  end

let f_body {Typing.prms;throws;locals;stms;return} =
  let (prms, cnt, lenv) = List.fold_left (fun (prms, cnt, lenv) (t, id) ->
    ((t, id, cnt)::prms, cnt + 1, LocalsEnv.add id.Ast.id cnt lenv)
  ) ([], 1, LocalsEnv.empty) prms in
  let (locals, _, lenv) = List.fold_left (fun (locals, cnt, lenv) (t, id, e) ->
    let prm = (t, id, f_exp lenv e, cnt) in
    (prm::locals, cnt + 1, LocalsEnv.add id.Ast.id cnt lenv)
  ) ([], cnt, lenv) locals in
  { prms   = List.rev prms;
    throws;
    locals = List.rev locals;
    stms   = List.map (f_stm lenv) stms;
    return = f_rstm lenv return; }

let make_msig cname name prms return_sig =
  let prm_sigs = List.map (fun (t,_) -> t_to_sig t) prms in
  cname ^ "/" ^ name ^ "(" ^ (String.concat "" prm_sigs) ^ ")" ^ return_sig 

let f_field cname = function
  | Typing.Method(t, ({Ast.id} as name), ({Typing.prms} as body)) ->
    Method(t, name, f_body body, make_msig cname id prms (t_to_sig t))
  | Typing.Constructor (name, body) ->
    Constructor (name, f_body body, make_msig cname "<init>" body.Typing.prms "V")
  | Typing.Main (body) -> Main(f_body body)
  | Typing.Field (t, name, init) ->
    (* Note: no locals in opt. field initializer *)
    Field(t, name, Types.opt (f_exp LocalsEnv.empty) init, cname ^ "/" ^ name.Ast.id)

let rec f prog =
  List.map (fun {Typing.cfilename;cname;cfields} ->
    let {Ast.id} = cname in
    { cfilename; cname;
      cfields = List.map (f_field id) cfields;
      csig    = id;
    }
  ) prog

