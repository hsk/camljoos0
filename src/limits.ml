(** Compiler phase to calculate the maximum number of locals and temporary stack
    locations needed for each method. *)

type id = Ast.id
type t = Types.t
type prm = t * id * int (*NEW*)
(*type local   = t * id (* * exp option*) * int (*NEW*)*)

type body =
    { limits : int * int; (*NEW*)
    (*prms   : prm list;*)
    (*locals : local list;*)
      body   : Inst.instruction list;
    }

type field =
  | Constructor of id * body * string
  | Method of t * id * body * string
  | Main of body
  | Field of t * id * (*field_init      : exp option;*) string

type class_decl =
    { cfilename : string;
      cname     : id;
      cfields   : field list;
      csig      : string (*NEW*) }


module LabelMap = Map.Make(String)

let error msg = raise (Error.InternalCompilerError msg)

let f_body {Codegen.prms;body} =

  (* label map that associates instruction sequences to labels *)
  let lbl2cont_insts =
    let rec loop env = function
      | []                     -> env
      | Inst.Ilabel lbl::insts -> loop (LabelMap.add lbl insts env) insts
      | _::insts               -> loop env insts
    in
    loop LabelMap.empty body
  in

  (* graph traversal of control-flow graph *)
  let rec loop s v env = function
    | [] -> (v, env)
    | Inst.Ilabel lbl :: insts ->
      if not (LabelMap.mem lbl env) then loop s v (LabelMap.add lbl s env) insts else
      let h = LabelMap.find lbl env in
      if h != s then error (Printf.sprintf "Stack height does not match at %s: (%d != %d)" lbl h s);
      loop s v env insts
    | inst :: insts ->
      let s = s + (Inst.stack_change inst) in
      if s < 0 then error (Printf.sprintf "Negative stack height at %s (%d)" (Inst.to_asm inst) s);
      let v = max v s in
      (* then continue along path *)
      let (v, env) = if not (Inst.can_fall_through inst) then (v, env) else loop s v env insts in
      (* then explore alternative path *)
      begin match Inst.can_jump inst with
      | None -> (v, env)
      | Some lbl ->
        if not (LabelMap.mem lbl env) then
          loop s v (LabelMap.add lbl s env) (LabelMap.find lbl lbl2cont_insts) else
        (* been here: check consistency *)
        let h = LabelMap.find lbl env in
        if h != s then error (Printf.sprintf "Stack height does not match at %s: (%d != %d)" lbl h s);
        (v, env)
      end
  in
  let (max_stack, _) = loop 0 0 LabelMap.empty body in
  let max_locals = 
    List.fold_left (fun v i ->
      match Inst.local_access i with
      | None -> v
      | Some l -> max (l + 1) v
    ) (List.length prms + 1) body
  in
  {limits = (max_stack, max_locals); body}

let f_field = function
  | Codegen.Method(t, name, body, jsig) -> Method(t, name, f_body body, jsig)
  | Codegen.Constructor(name, body, jsig) -> Constructor(name, f_body body, jsig)
  | Codegen.Main body -> Main(f_body body)
  | Codegen.Field(t, name, jsig) -> Field(t, name, jsig)

let f prog =
  List.map (fun {Codegen.cfilename;cname;cfields;csig} ->
    { cfilename; cname; csig; cfields = List.map f_field cfields; }
  ) prog

