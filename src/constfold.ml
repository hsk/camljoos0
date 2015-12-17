open Typing

let rec f_exp e = 
  let mk exp = {e with exp} in
  mk begin match e.exp with
  | Binop (e1, op, e2) ->
    let e1 = f_exp e1 in
    let e2 = f_exp e2 in
    begin match (e1.exp, e2.exp) with
    | (IntConst l, IntConst r) ->
      begin match op with
      | Add -> IntConst (Int32.add l r)
      | Sub -> IntConst (Int32.sub l r)
      | Mul -> IntConst (Int32.mul l r)
      | Div -> if r = 0l then e.exp else IntConst (Int32.div l r)
      | Mod -> if r = 0l then e.exp else IntConst (Int32.rem l r)
      | Eq  -> BooleanConst (l =  r)
      | Ne  -> BooleanConst (l <> r)
      | Lt  -> BooleanConst (l <  r)
      | Le  -> BooleanConst (l <= r)
      | Gt  -> BooleanConst (l >  r)
      | Ge  -> BooleanConst (l >= r)
      | _   -> raise (Error.InternalCompilerError "Invalid op for int folding")
      end
    | (BooleanConst l, BooleanConst r) ->
      begin match op with
      | Eq  -> BooleanConst (l =  r)
      | Ne  -> BooleanConst (l <> r)
      | And -> BooleanConst (l && r)
      | Or  -> BooleanConst (l || r)
      | Xor -> BooleanConst (l <> r)
      | _   -> raise (Error.InternalCompilerError "Invalid op for boolean folding")
      end
    | (StringConst l, StringConst r) ->
      begin match op with
      | Cat -> StringConst  (l ^  r)
      | Aeq -> BooleanConst (l =  r)
      | Ane -> BooleanConst (l <> r)
      | _   -> raise (Error.InternalCompilerError "Invalid op for string folding")
      end
    | _ -> Binop (e1,op,e2)
    end
  | Unop (op,e) ->
    let e = f_exp e in
    begin match (op, e.exp) with
    | (Negate,          IntConst i)     -> IntConst (Int32.neg i)
    | (Complement,      BooleanConst b) -> BooleanConst (not b)
    | (BooleanToString, BooleanConst b) -> StringConst (string_of_bool b)
    | (CharToString,    IntConst i)     -> StringConst (String.make 1 (Char.chr (Int32.to_int i)))
    | (IntToString,     IntConst i)     -> StringConst (Int32.to_string i)
    | (ObjectToString,  _)
    | (_,               _)              -> Unop (op, e)
    end
  | StringConst s -> StringConst (String.sub s 1 (String.length s - 2)) (*do not include ""*)
  | IntConst _
  | BooleanConst _
  | Null
  | This -> e.exp
  | Invoke (e, id, es, mt) -> Invoke (f_exp e, id, List.map f_exp es, mt)
  | New (id, es, ct) -> New (id, List.map f_exp es, ct)
  | Lvalue _ -> e.exp
  | Assignment (lval, e) -> Assignment (lval, f_exp e)
  | Print e -> Print (f_exp e)
  | Read -> e.exp
  end

let rec f_stm stm = 
  let mk s = { stm with stm = s } in
  mk begin match stm.stm with
  | Exp e -> Exp (f_exp e)
  | If (e, s1, s2) -> If (f_exp e, f_stm s1, f_stm s2)
  | While (e, s) ->
    let e = f_exp e in
    begin match e.exp with
    | BooleanConst b -> Error.error e.e_pos "Constant while condition not allowed"
    | _ -> While (e, f_stm s)
    end
  | Empty -> Empty
  | Block stms -> Block (List.map f_stm stms)
  end

let f_rstm ({rstm} as s) =
  match rstm with
  | VoidReturn      -> s
  | ValueReturn e -> { s with rstm = ValueReturn (f_exp e) }

let f_body {prms;throws;locals;stms;return} =
  {
    prms; throws;
    locals = List.map (fun (t, id, e) -> (t, id, f_exp e)) locals;
    stms   = List.map f_stm stms;
    return = f_rstm return
  }

let f_field = function
  | Method(t, name, body) -> Method(t, name, f_body body)
  | Constructor(name, body) -> Constructor(name, f_body body)
  | Main body -> Main(f_body body)
  | Field(t, name, init) -> Field(t, name, Types.opt f_exp init)

let rec f prog =
  List.map (fun {cfilename; cname; cfields} ->
    {cfilename; cname; cfields = List.map f_field cfields}
  ) prog

