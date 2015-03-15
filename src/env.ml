open Ast

module M = Types.M

let lookup_env env kind id pos =
  if M.mem id env then M.find id env
  else Error.error pos "The %s '%s' is not defined." kind id

let f_t {t} =
  match t with
  | Void     -> Types.Void
  | Int      -> Types.Int
  | Boolean  -> Types.Boolean
  | String   -> Types.String
  | Class id -> Types.Class (id.id)

let f_prm lenv (typ, {id;id_pos}) =
  if M.mem id lenv then Error.error id_pos "The variable '%s' is already defined." id;
  let ltyp = f_t typ in
  (ltyp, M.add id ltyp lenv)

let f_local lenv (typ, id, _) =
  f_prm lenv (typ, id)

let f_field menv = function
  | Field(t, {id;id_pos}, _) ->
    if M.mem id menv then Error.error id_pos "The field '%s' is already defined." id;
    let field = {Types.field_type = f_t t; field_name = id} in
    (field, M.add id field menv)
  | _ -> assert false

let rec fold f env = function
  | [] -> ([], env)
  | x::xs ->
    let (x, env)  = f env x in
    let (xs, env) = fold f env xs in
    (x::xs, env)

let f_body {prms; locals} =
  let (ftypes, lenv) = fold f_prm M.empty prms in
  ignore (fold f_local lenv locals);
  ftypes

let f_constructor = function
  | Constructor(name, body) -> (name.id, f_body body)
  | _ -> assert false

let f_method menv = function
  | Method(t, {id; id_pos}, body) ->
    if M.mem id menv then Error.error id_pos "The method '%s' is already defined." id;
    let mdecl = {Types.mresult = f_t t; mname = id; mprms = f_body body;} in
    (mdecl, M.add id mdecl menv)
  | _ -> assert false

let f prog =
  List.fold_left (fun env {cfilename;cname={id;id_pos};cfields} ->
    if M.mem id env then Error.error id_pos "The class '%s' is already defined." id;
    let (fields, methods) = List.partition(function Field _ -> true | _ -> false) cfields in
    let constructor = List.find (function Constructor _ -> true | _ -> false ) methods in
    let methods = List.filter (function Method _ -> true | _ -> false) methods in
    let (mdecls,_) = fold f_method M.empty methods in
    let (fields,_) = fold f_field M.empty fields in
    let typ = 
      { Types.cname  = id;
        cfields      = fields;
        cconstruct = f_constructor constructor;
        cmethods = mdecls }
    in M.add id typ env
  ) M.empty prog

