module TAst = Typecheckingast
module Inst = Instruction

type identifier = Ast.identifier


type typeexp = Types.typeexp


type formal_param = typeexp * identifier * int (*NEW*)
(*type local_decl   = typeexp * identifier (* * exp *) * int (*NEW*)*)

type field_decl	=	
    { field_type      : typeexp;
      field_name      : identifier;
      (*field_init      : exp option;*)
      field_signature : string (*NEW*) }

type body =
    { formals    : formal_param list;
(*    locals     : local_decl list; *)
      body       : Inst.instruction list; }

type constructor_decl =
    { constructor_name             : identifier;
      constructor_body : body;
      constructor_signature        : string (*NEW*) }

type main_decl = body

type method_decl =
    { method_return_type      : typeexp;
      method_name             : identifier;
      method_body : body;
      method_signature        : string (*NEW*) }


type class_decl	=
    { source_file_name : string;
      class_name           : identifier;
      class_fields         : field_decl list;
      class_constructor    : constructor_decl;
      class_main           : main_decl option;
      class_methods        : method_decl list;
      class_decl_signature : string (*NEW*) }

type program = class_decl list
