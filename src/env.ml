open Ast

module M = Types.M

let lookup_env env kind id pos =
  if M.mem id env
  then M.find id env
  else Error.error pos "The %s '%s' is not defined." kind id

let f_t {t} =
  match t with
  | Void     -> Types.Void
  | Int      -> Types.Int
  | Boolean  -> Types.Boolean
  | String   -> Types.String
  | Class id -> Types.Class (id.id)

let f_formal lenv (typ, {id=name;id_pos=pos}) =
  if M.mem name lenv then Error.error pos "The variable '%s' is already defined." name;
  let ltyp = f_t typ in
  (ltyp, M.add name ltyp lenv)

let f_local lenv (typ, id, _) =
  f_formal lenv (typ, id)

let f_field menv = function
  | Field(ty, {id=field_name;id_pos=pos}, _) ->
    if M.mem field_name menv then 
      Error.error pos "The field '%s' is already defined." field_name;
    let field = { Types.field_type = f_t ty; field_name} in
    (field, M.add field_name field menv)
  | _ -> assert false

let rec fold f env = function
  | [] -> ([], env)
  | x::xs ->
    let (x, env)   = f env x in
    let (xs, env)  = fold f env xs in
    (x::xs, env)

let f_body {formals;locals} =
  let (ftypes,lenv)  = fold f_formal M.empty formals in
  let (_     , _)    = fold f_local lenv locals in
  ftypes

let f_constructor = function
  | Constructor(name, body) -> (name.id, f_body body)
  | _ -> assert false

let f_method menv = function
  | Method(t, name, body) ->
    if M.mem name.id menv then 
      Error.error name.id_pos "The method '%s' is already defined." name.id;
    let mdecl   = { Types.method_result  = f_t t;
                    method_name    = name.id;
                    method_formals = f_body body; } in
    (mdecl, M.add name.id mdecl menv)
  | _ -> assert false

let f prog =
  List.fold_left (fun env {cfilename;cname={id=name;id_pos=pos};cfields} ->
    if M.mem name env then Error.error pos "The class '%s' is already defined." name;
    let (fields, methods) = List.partition(function Field _ -> true | _ -> false) cfields in
    let constructor = List.find (function Constructor _ -> true | _ -> false ) methods in
    let methods = List.filter (function Method _ -> true | _ -> false) methods in
    let (mdecls,_) = fold f_method M.empty methods in
    let (fields,_) = fold f_field M.empty fields in
    let typ = 
      { Types.cname  = name;
        cfields      = fields;
        cconstruct = f_constructor constructor;
        class_methods     = mdecls }
    in M.add name typ env
  ) M.empty prog

