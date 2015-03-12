(** Compiler phase to calculate the maximum number of locals and temporary stack
    locations needed for each method. *)

module CAst = Codegenast
module LAst = Limitsast
module Inst = Instruction

module LabelMap = Map.Make(String)


let verify_error message =
  raise (Error.InternalCompilerError message)


type stackinfo = { maxstack : int; stackmap : int LabelMap.t }


let limit_body fab =
  let formals = fab.CAst.formals in
  let body    = fab.CAst.body in

  (* find maximum local by iterating over the instructions of the body *)
  let mymax = 
    let rec find_max_local is max =
      match is with
      | [] -> max
      | i::is -> 
        find_max_local is (match Inst.local_access i with
          | None -> max
          | Some l -> if l+1 > max then l+1 else max
        )
    in 
    find_max_local body (List.length formals + 1)
  in
  (* label map that associates instruction sequences to labels *)
  let lmap =
    let rec build_label_map lmap = function
      | [] -> lmap
      | i::is ->
        let lmap =
          match i with
          | Inst.Ilabel l -> LabelMap.add l is lmap
          | _ -> lmap
        in
        build_label_map lmap is
    in
    build_label_map LabelMap.empty body
  in

  (* graph traversal of control-flow graph *)
  let rec traverse is stack stackinfo =
    match is with
    | [] -> stackinfo
    | i::is -> 
      begin match i with
        | Inst.Ilabel l ->
          let stackinfo = 
            if LabelMap.mem l stackinfo.stackmap then (
              let height = LabelMap.find l stackinfo.stackmap in
              if height != stack then
                verify_error ("Stack height does not match at " ^ l ^ ": (" ^
                              (string_of_int height) ^ " != " ^ (string_of_int stack) ^ ")");
              stackinfo
            ) else
            { stackinfo with stackmap = LabelMap.add l stack stackinfo.stackmap }
          in
          traverse is stack stackinfo
        | _ -> 
          let stack = stack + (Inst.stack_change i) in
          let stackinfo =
            { stackinfo with
              maxstack = if stack > stackinfo.maxstack then stack else stackinfo.maxstack }
          in
          if stack < 0 then
            verify_error ("Negative stack height at " ^ (Inst.to_asm i) ^
                " (" ^ (string_of_int stack) ^ ")");
          let stackinfo = 
            if Inst.can_fall_through i  (* then continue along path *)
            then traverse is stack stackinfo
            else stackinfo
          in
          begin match Inst.can_jump i with   (* then explore alternative path *)
            | None -> stackinfo
            | Some l ->
              if LabelMap.mem l stackinfo.stackmap then (
                let height = LabelMap.find l stackinfo.stackmap in (* been here: check consistency *)
                if height != stack
                then verify_error ("Stack height does not match at " ^ l ^ ": (" ^
                  (string_of_int height) ^ " != " ^ (string_of_int stack) ^ ")");
                stackinfo
              ) else
              let is = LabelMap.find l lmap in   (* first time here: "color" l *)
              let stackinfo =
                { stackinfo with stackmap = LabelMap.add l stack stackinfo.stackmap } in
              traverse is stack stackinfo
          end
      end
  in

  let stackinfo = traverse body 0 { maxstack = 0; stackmap = LabelMap.empty } in

  { LAst.formals      = formals;
    LAst.body         = body;
    LAst.max_stack    = stackinfo.maxstack;
    LAst.max_locals   = mymax  }


let limit_field fdecl =
    { LAst.field_type      = fdecl.CAst.field_type;
      LAst.field_name      = fdecl.CAst.field_name;
      LAst.field_signature = fdecl.CAst.field_signature }


let limit_method mdecl =
  { LAst.method_return_type = mdecl.CAst.method_return_type;
    LAst.method_name        = mdecl.CAst.method_name;
    LAst.method_body        = limit_body mdecl.CAst.method_body;
    LAst.method_signature   = mdecl.CAst.method_signature }


let limit_constructor cdecl =
  { LAst.constructor_name      = cdecl.CAst.constructor_name;
    LAst.constructor_body      = limit_body cdecl.CAst.constructor_body;
    LAst.constructor_signature = cdecl.CAst.constructor_signature }


let limit_program prog =
  List.map (fun cdecl ->
    let methods = List.map limit_method cdecl.CAst.class_methods in
    let fields  = List.map limit_field cdecl.CAst.class_fields in
    { LAst.source_file_name     = cdecl.CAst.source_file_name;
      LAst.class_name           = cdecl.CAst.class_name;
      LAst.class_fields         = fields;
      LAst.class_constructor    = limit_constructor cdecl.CAst.class_constructor;
      LAst.class_main           = Utils.opt limit_body cdecl.CAst.class_main;
      LAst.class_methods        = methods;
      LAst.class_decl_signature = cdecl.CAst.class_decl_signature }
  ) prog

