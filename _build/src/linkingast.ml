type identifier = Ast.identifier


type typeexp = Types.typeexp


type binop = Ast.binop
type unop = Ast.binop


type lvalue = Ast.lvalue
type exp    = Ast.exp


type stm        = Ast.stm
type return_stm = Ast.return_stm


type formal_param = typeexp * identifier
type local_decl   = typeexp * identifier * exp

type field_decl	=	
    { field_type : typeexp;
      field_name : identifier;
      field_init : exp option; }

type body =
    { formals    : formal_param list;
      locals     : local_decl list;
      statements : stm list;
      return     : return_stm; }

type constructor_decl =
    { constructor_name : identifier;
      constructor_body : body; }

type main_decl = body

type method_decl =
    { method_return_type : typeexp;
      method_name        : identifier;
      method_body        : body; }


type class_decl	=
    { 
      source_file_name  : string;
      class_name        : identifier;
      class_fields      : field_decl list;
      class_constructor : constructor_decl;
      class_main        : main_decl option;
      class_methods     : method_decl list; }


type program = class_decl list

