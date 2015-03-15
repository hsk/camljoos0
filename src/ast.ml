type id = { id_pos : Lexing.position; id : string } 

type t = { t_pos : Lexing.position; t : t_desc }
and t_desc =
  | Void
  | Int
  | Boolean
  | String
  | Class of id

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
  | Local of id
  | Field of id

type exp = { exp_pos: Lexing.position; exp: exp_desc }
and exp_desc =
  | Binop of exp * binop * exp
  | Unop of unop * exp
  | IntConst of int32
  | StringConst of string
  | BooleanConst of bool
  | Null
  | This
  | Invoke of exp * id * exp list
  | New of id * exp list
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

type formal_param = t * id
type local_decl   = t * id * exp

type body =
    { formals : formal_param list;
      locals  : local_decl list;
      stms    : stm list;
      return  : return_stm; }

type field_decl = 
  | Method of t * id * body
  | Constructor of id * body
  | Main of body
  | Field of t * id * exp option

type class_decl	=
    {
      cfilename : string;
      cname      : id;
      cfields    : field_decl list;
    }

