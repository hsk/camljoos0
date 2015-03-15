
module M = Map.Make (String)

type t = 
  | Void
  | Int
  | Boolean
  | String
  | Class of string
  | Null

type ft =
    { ft     : t;
      field_name     : string; }

type mt =
    { mresult : t;
      mname   : string;
      mprms   : t list; }

type constructor_t = string * t list

type class_type =
    { cname      : string;
      cfts       : ft list;
      cconstruct : constructor_t;
    (*class_main : mt option;*)
      cmts       : mt list; }

(* map from identifier to named_type *)
(* type type_env = class_type Env.t *)

let dots_to_slashes s =
  let dotregexp = Str.regexp "\\." in
  let ss = Str.split dotregexp s in
  String.concat "/" ss

let cname_to_sig cn = dots_to_slashes cn

let rec t_to_string = function
  | Void    -> "V"
  | Int     -> "I"
  | Boolean -> "Z"
  | String  -> "java.lang.String"
  | Class c -> (*cname_to_sig*) c
  | Null    -> raise (Error.InternalCompilerError "The null type has no signature")

let rec t_to_sig t = match t with
  | Void
  | Int
  | Boolean -> t_to_string t
  | String
  | Class _ -> "L" ^ (cname_to_sig (t_to_string t)) ^ ";"
  | Null    -> raise (Error.InternalCompilerError "The null type has no signature")
