type identifier = { identifier_pos : Lexing.position; identifier : string } 

type typeexp = { typeexp_pos : Lexing.position; typeexp : typeexp_desc }
and typeexp_desc =
  | Void
  | Int
  | Boolean
  | String
  | Class of identifier

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
  | Concat (* // Only created for + (char) *)

type unop =
  | Negate     (*minus*)
  | Complement (*complement*)
  | CharToString

type lvalue = { lvalue_pos: Lexing.position; lvalue: lvalue_desc }
and lvalue_desc =
  | Local of identifier
  | Field of identifier

type exp = { exp_pos: Lexing.position; exp: exp_desc }
and exp_desc =
  | Binop of exp * binop * exp
  | Unop of unop * exp
  | IntConst of int32
  | StringConst of string
  | BooleanConst of bool
  | Null
  | This
  | Invoke of exp * identifier * exp list
  | New of identifier * exp list
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

type formal_param = typeexp * identifier
type local_decl   = typeexp * identifier * exp

type body =
    { formals    : formal_param list;
      locals     : local_decl list;
      statements : stm list;
      return     : return_stm; }

type field_decl = 
  | Method of typeexp * identifier * body
  | Constructor of identifier * body
  | Main of body
  | Field of typeexp * identifier * exp option

type class_decl	=
    {
      source_file       : string;
      class_name        : identifier;
      class_fields      : field_decl list;
    }

type program = class_decl list
