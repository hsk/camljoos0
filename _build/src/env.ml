module M = Types.M

let lookup_env env kind id pos =
  if M.mem id env
  then M.find id env
  else Error.error pos ("The " ^ kind ^ " '" ^ id ^ "' is not defined.")


let env_typeexp texp =
  match texp.Ast.typeexp with
  | Ast.Void     -> Types.Void
  | Ast.Int      -> Types.Int
  | Ast.Boolean  -> Types.Boolean
  | Ast.String   -> Types.String
  | Ast.Class id -> Types.Class (id.Ast.identifier)


let env_formal_param lenv (typ, id) =
  let varname = id.Ast.identifier in
  if M.mem varname lenv then 
    let pos = id.Ast.identifier_pos in
    Error.error pos ("The variable '" ^ varname ^ "' is already defined.") else 
  let ltyp = env_typeexp typ in
  (ltyp, M.add varname ltyp lenv)


let env_local lenv (typ, id, _) =
  env_formal_param lenv (typ, id)


let env_field fenv field_decl =
  let fieldname = field_decl.Ast.field_name.Ast.identifier in
  if M.mem fieldname fenv then 
    let pos = field_decl.Ast.field_name.Ast.identifier_pos in
    Error.error pos ("The field '" ^ fieldname ^ "' is already defined.") else
  let field = { Types.field_type = env_typeexp field_decl.Ast.field_type;
                Types.field_name = fieldname; } in
  (field, M.add fieldname field fenv)


let env_body body =
  let (ftypes,lenv)  = Utils.fold env_formal_param M.empty body.Ast.formals in
  let (_     , _)    = Utils.fold env_local lenv body.Ast.locals in
  ftypes


let env_constructor cdecl = 
  { Types.constructor_name    = cdecl.Ast.constructor_name.Ast.identifier;
    Types.constructor_formals = env_body cdecl.Ast.constructor_body }


let env_method menv mdecl =
  let methodname = mdecl.Ast.method_name.Ast.identifier in
  if M.mem methodname menv then 
    let pos = mdecl.Ast.method_name.Ast.identifier_pos in
    Error.error pos ("The method '" ^ methodname ^ "' is already defined.") else
  let mdecl   = { Types.method_result  = env_typeexp mdecl.Ast.method_return_type;
                  Types.method_name    = methodname;
                  Types.method_formals = env_body mdecl.Ast.method_body; } in
  (mdecl, M.add methodname mdecl menv)


let env_program prog =
  List.fold_left (fun env decl ->
    let name = decl.Ast.class_name.Ast.identifier in
    if M.mem name env then
      let pos = decl.Ast.class_name.Ast.identifier_pos in
      Error.error pos ("The class '" ^ name ^ "' is already defined.") else

    let (mdecls,_) = Utils.fold env_method M.empty decl.Ast.class_methods in
    let (fields,_) = Utils.fold env_field M.empty decl.Ast.class_fields in
    let typ = 
      { Types.class_name        = name;
        Types.class_fields      = fields;
        Types.class_constructor = env_constructor decl.Ast.class_constructor;
        Types.class_methods     = mdecls }
    in M.add name typ env
  ) M.empty prog

