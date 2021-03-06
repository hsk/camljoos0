type id = { id_pos : Lexing.position; id : string } 

type t = { t_pos : Lexing.position; t : t_desc }
and t_desc =
  | TVoid
  | TInt
  | TBoolean
  | TString
  | TClass of id

type binop =
  | Add | Sub
  | Mul | Div | Mod
  | Eq  | Ne
  | Lt  | Le
  | Gt  | Ge
  | And | Or | Xor
  | Cat (* // Only created for + (char) *)

type unop =
  | Negate     (*minus*)
  | Complement (*complement*)
  | CharToString

type lval = { lpos: Lexing.position; lval: lval_desc }
and lval_desc =
  | Local of id
  | Field of id

type exp = { e_pos: Lexing.position; exp: exp_desc }
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
  | Lvalue of lval
  | Assignment of lval * exp
  | Print of exp
  | Read


type stm = { stm_pos: Lexing.position; stm: stm_desc }
and stm_desc =
  | Exp of exp
  | If of exp * stm * stm
  | While of exp * stm
  | Empty
  | Block of stm list

type rstm = { rstm_pos: Lexing.position; rstm: rstm_desc }
and rstm_desc = 
  | VoidReturn
  | ValueReturn of exp

type prm   = t * id
type local = t * id * exp

type body =
    { prms   : prm list;
      locals : local list;
      throws : string list;
      stms   : stm list;
      return : rstm; }

type field = 
  | Method of t * id * body
  | Constructor of id * body
  | Main of body
  | Field of t * id * exp option

type class_decl	=
  {
    cfilename : string;
    cname     : id;
    cfields   : field list;
  }

