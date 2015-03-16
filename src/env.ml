open Ast
open Types

let lookup_env env kind id pos =
  if M.mem id env then M.find id env
  else Error.error pos "The %s '%s' is not defined." kind id

let rec fold f env xs = 
  let (xs, env) = List.fold_left (fun (xs, env) x ->
    let (x, env) = f env x in
    (x::xs, env)
  ) ([],env) xs in
  (List.rev xs, env)

let f_t {t} =
  match t with
  | Ast.TVoid       -> Types.TVoid
  | Ast.TInt        -> Types.TInt
  | Ast.TBoolean    -> Types.TBool
  | Ast.TString     -> Types.TString
  | Ast.TClass {id} -> Types.TClass id

let f_field menv = function
  | Field(t, {id;id_pos}, _) ->
    if M.mem id menv then Error.error id_pos "The field '%s' is already defined." id;
    let ft = {ft = f_t t; ft_name = id} in
    (ft, M.add id ft menv)
  | _ -> assert false

let f_body {prms; locals} =
  let f_prm lenv (t, {id;id_pos}) =
    if M.mem id lenv then Error.error id_pos "The variable '%s' is already defined." id;
    let t = f_t t in
    (t, M.add id t lenv)
  in
  let (fts, lenv) = fold f_prm M.empty prms in
  ignore (fold (fun lenv (t, id, _) -> f_prm lenv (t, id)) lenv locals);
  fts

let f_constructor = function
  | Constructor({id}, body) -> (id, f_body body)
  | _ -> assert false

let f_method menv = function
  | Method(t, {id; id_pos}, body) ->
    if M.mem id menv then Error.error id_pos "The method '%s' is already defined." id;
    let mt = {mresult_t = f_t t; mname_t = id; mprms_t = f_body body;} in
    (mt, M.add id mt menv)
  | _ -> assert false

let f prog =
  List.fold_left (fun env {cfilename; cname={id;id_pos}; cfields} ->
    if M.mem id env then Error.error id_pos "The class '%s' is already defined." id;
    let (fields, methods) = List.partition(function Field _ -> true | _ -> false) cfields in
    let constructor = List.find (function Constructor _ -> true | _ -> false ) methods in
    let methods = List.filter (function Method _ -> true | _ -> false) methods in
    let (cmethod_ts, _) = fold f_method M.empty methods in
    let (cfield_ts, _) = fold f_field M.empty fields in
    let t = 
      {cname_t = id; cfield_ts; cconstruct_t = f_constructor constructor; cmethod_ts}
    in M.add id t env
  ) M.empty prog

