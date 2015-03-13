open Ast

module M = Types.M

let lookup_env env kind id pos =
  if M.mem id env
  then M.find id env
  else Error.error pos ("The " ^ kind ^ " '" ^ id ^ "' is not defined.")


let env_typeexp {typeexp} =
  match typeexp with
  | Void     -> Types.Void
  | Int      -> Types.Int
  | Boolean  -> Types.Boolean
  | String   -> Types.String
  | Class id -> Types.Class (id.identifier)


let env_formal_param lenv (typ, id) =
  let varname = id.identifier in
  if M.mem varname lenv then 
    let pos = id.identifier_pos in
    Error.error pos ("The variable '" ^ varname ^ "' is already defined.") else 
  let ltyp = env_typeexp typ in
  (ltyp, M.add varname ltyp lenv)


let env_local lenv (typ, id, _) =
  env_formal_param lenv (typ, id)


let env_field fenv {field_name;field_type} =
  let fieldname = field_name.identifier in
  if M.mem fieldname fenv then 
    let pos = field_name.identifier_pos in
    Error.error pos ("The field '" ^ fieldname ^ "' is already defined.") else
  let field = { Types.field_type = env_typeexp field_type;
                Types.field_name = fieldname; } in
  (field, M.add fieldname field fenv)


let env_body {formals;locals} =
  let (ftypes,lenv)  = Utils.fold env_formal_param M.empty formals in
  let (_     , _)    = Utils.fold env_local lenv locals in
  ftypes


let env_constructor (name, body) = (name.identifier, env_body body)


let env_method menv {method_name;method_return_type;method_body} =
  let methodname = method_name.identifier in
  if M.mem methodname menv then 
    let pos = method_name.identifier_pos in
    Error.error pos ("The method '" ^ methodname ^ "' is already defined.") else
  let mdecl   = { Types.method_result  = env_typeexp method_return_type;
                  Types.method_name    = methodname;
                  Types.method_formals = env_body method_body; } in
  (mdecl, M.add methodname mdecl menv)


let env_program prog =
  List.fold_left (fun env {class_name;class_methods;class_fields;class_constructor} ->
    let name = class_name.identifier in
    if M.mem name env then
      let pos = class_name.identifier_pos in
      Error.error pos ("The class '" ^ name ^ "' is already defined.") else

    let (mdecls,_) = Utils.fold env_method M.empty class_methods in
    let (fields,_) = Utils.fold env_field M.empty class_fields in
    let typ = 
      { Types.class_name        = name;
        Types.class_fields      = fields;
        Types.class_constructor = env_constructor class_constructor;
        Types.class_methods     = mdecls }
    in M.add name typ env
  ) M.empty prog

