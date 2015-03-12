module TAst = Typecheckingast

type identifier = Ast.identifier


type typeexp = Types.typeexp


type binop = TAst.binop
type unop = TAst.unop


type lvalue = { lvalue_pos: Lexing.position; lvalue_type : Types.typeexp; lvalue: lvalue_desc }
and lvalue_desc =
  | Local of identifier * int (*NEW*)
  | Field of identifier * Types.field_type


type exp = { exp_pos: Lexing.position; exp_type : Types.typeexp; exp: exp_desc }
and exp_desc =
  | Binop of exp * binop * exp
  | Unop of unop * exp
  | IntConst of int32
  | StringConst of string
  | BooleanConst of bool
  | Null
  | This
  | Invoke of exp * identifier * exp list * Types.method_type (*NEW*)
  | New of identifier * exp list * Types.constructor_type (*NEW*)
  | Lvalue of lvalue
  | Assignment of lvalue * exp
  | Print of exp
  | Read


type stm = { stm_pos: Lexing.position; stm: stm_desc }
and stm_desc =
  | Exp of exp
  | IfThen of exp * stm
  | IfThenElse of exp * stm * stm
  | While of exp * stm
  | Empty
  | Block of stm list

type return_stm = { return_stm_pos: Lexing.position; return_stm: return_stm_desc }
and return_stm_desc = 
  | VoidReturn
  | ValueReturn of exp


type formal_param = typeexp * identifier * int (*NEW*)
type local_decl   = typeexp * identifier * exp * int (*NEW*)

type field_decl	=	
    { field_type      : typeexp;
      field_name      : identifier;
      field_init      : exp option;
      field_signature : string (*NEW*) }

type body =
    { formals    : formal_param list;
      locals     : local_decl list;
      statements : stm list;
      return     : return_stm; }

type constructor_decl =
    { constructor_name      : identifier;
      constructor_body      : body;
      constructor_signature : string (*NEW*) }


type method_decl =
    { method_return_type    : typeexp;
      method_name           : identifier;
      method_body           : body;
      method_signature      : string (*NEW*) }


type class_decl	=
    { source_file_name : string;
      class_name           : identifier;
      class_fields         : field_decl list;
      class_constructor    : constructor_decl;
      class_main           : body option;
      class_methods        : method_decl list;
      class_decl_signature : string (*NEW*) }


type program = class_decl list

