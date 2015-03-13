(** Compiler phase to calculate the maximum number of locals and temporary stack
    locations needed for each method. *)

type identifier = Ast.identifier


type typeexp = Types.typeexp


type formal_param = typeexp * identifier * int (*NEW*)
(*type local_decl   = typeexp * identifier (* * exp option*) * int (*NEW*)*)

type field_decl = typeexp * identifier * (*field_init      : exp option;*) string

type body =
    { formals      : formal_param list;
      (*locals     : local_decl list;*)
      body         : Inst.instruction list;
      max_stack    : int; (*NEW*)
      max_locals   : int  (*NEW*) }

type constructor_decl = identifier * body * string

type method_decl =
    { method_return_type : typeexp;
      method_name        : identifier;
      method_body        : body;
      method_signature   : string (*NEW*) }


type class_decl =
    { source_file_name     : string;
      class_name           : identifier;
      class_fields         : field_decl list;
      class_constructor    : constructor_decl;
      class_main           : body option;
      class_methods        : method_decl list;
      class_decl_signature : string (*NEW*) }

type program = class_decl list

module LabelMap = Map.Make(String)


let verify_error message =
  raise (Error.InternalCompilerError message)


type stackinfo = { maxstack : int; stackmap : int LabelMap.t }


let limit_body {Codegen.formals;body} =

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

  { formals      = formals;
    body         = body;
    max_stack    = stackinfo.maxstack;
    max_locals   = mymax  }


let limit_field (ty, name, signature) =
    (ty, name, signature)


let limit_method {Codegen.method_return_type;method_name;method_body;method_signature} =
  { method_return_type = method_return_type;
    method_name        = method_name;
    method_body        = limit_body method_body;
    method_signature   = method_signature }


let limit_constructor (name, body, signature) =
  (name, limit_body body, signature)


let limit_program prog =
  List.map (fun {Codegen.class_methods;class_fields;source_file_name;
    class_name;class_constructor;class_main;class_decl_signature} ->
    { source_file_name     = source_file_name;
      class_name           = class_name;
      class_fields         = List.map limit_field class_fields;
      class_constructor    = limit_constructor class_constructor;
      class_main           = Utils.opt limit_body class_main;
      class_methods        = List.map limit_method class_methods;
      class_decl_signature = class_decl_signature }
  ) prog

