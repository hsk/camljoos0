
module M = Map.Make (String)

type t = 
  | TVoid
  | TInt
  | TBool
  | TString
  | TClass of string
  | TNull

type ft =
    { ft      : t;
      ft_name : string; }

type mt =
    { mresult_t : t;
      mname_t   : string;
      mprms_t   : t list; }

type constructor_t = string * t list

type class_type =
    { cname_t      : string;
      cfield_ts    : ft list;
      cconstruct_t : constructor_t;
      cmethod_ts   : mt list; }

(* map from identifier to named_type *)
(* type type_env = class_type Env.t *)

let dots_to_slashes s =
  let dotregexp = Str.regexp "\\." in
  let ss = Str.split dotregexp s in
  String.concat "/" ss

let cname_to_sig cn = dots_to_slashes cn

let rec show_t = function
  | TVoid    -> "V"
  | TInt     -> "I"
  | TBool    -> "Z"
  | TString  -> "java.lang.String"
  | TClass c -> (*cname_to_sig*) c
  | TNull    -> raise (Error.InternalCompilerError "The null type has no signature")

let rec t_to_sig t = match t with
  | TVoid
  | TInt
  | TBool    -> show_t t
  | TString
  | TClass _ -> "L" ^ (cname_to_sig (show_t t)) ^ ";"
  | TNull    -> raise (Error.InternalCompilerError "The null type has no signature")

let opt f = function
  | None -> None
  | Some a -> Some(f a)
