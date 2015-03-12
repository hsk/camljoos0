(* ********************************************************************** *)
(* AST TYPE FROM TYPECHECKING                                             *)
(* ********************************************************************** *)

type identifier = Ast.identifier

(* *************** Types *************** *)

type typeexp = Types.typeexp

(* *************** Operators *************** *)

type binop =
  | Add
  | Minus
  | Times
  | Divide
  | Modulo
  | Eq
  | Ne
  | Lt
  | Le
  | Gt
  | Ge
  | And
  | Or
  | Xor
  | Aeq  (*NEW*)
  | Ane  (*NEW*)
  | Concat (* Only created for + (char) *)

type unop =
  | Negate     (*minus*)
  | Complement (*complement*)
  | BooleanToString  (*NEW*)
  | IntToString      (*NEW*)
  | CharToString
  | ObjectToString   (*NEW*)


(* *************** Expressions *************** *)

type lvalue = { lvalue_pos: Lexing.position; lvalue_type : Types.typeexp; lvalue: lvalue_desc }
and lvalue_desc =
  | Local of identifier
  | Field of identifier * Types.field_type (*NEW*)


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


(* *************** Blocks and statements *************** *)

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


(* *************** Class members *************** *)

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


(* *************** Class and source file *************** *)

type class_decl	=
    { source_file_name  : string;
      class_name        : identifier;
      class_fields      : field_decl list;
      class_constructor : constructor_decl;
      class_main        : main_decl option;
      class_methods     : method_decl list; }

type program = class_decl list
