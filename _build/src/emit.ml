(** Final compiler phase to emit the generated code for all classes and
    interfaces to jasmin files. *)

module LAst = Limitsast
module Inst = Instruction


let output_line ch str =
  begin
    output_string ch str;
    output_char ch '\n';
    flush ch
  end

let output_newline ch =
  output_char ch '\n';
  flush ch

let make_sig method_name fullsig =
  method_name ^ (Str.string_after fullsig (String.index fullsig '('))


let emit_field ch fdecl =
  let field_name = fdecl.LAst.field_name.Ast.identifier in
  let field_type = Types.typeexp_to_sig fdecl.LAst.field_type in
  output_line ch (".field protected " ^ field_name ^ " " ^ field_type)


let emit_body ch fab = 
  output_line ch (".limit stack " ^ (string_of_int fab.LAst.max_stack));
  output_line ch (".limit locals " ^ (string_of_int fab.LAst.max_locals));
  List.iter (fun inst -> output_line ch (Inst.to_asm inst)) fab.LAst.body


let emit_main_opt ch = function
  | None -> ()
  | Some fab ->
      output_newline ch;
      output_line ch ".method public static main([Ljava/lang/String;)V";
      output_line ch ".throws java/lang/Exception";
      emit_body ch fab;
      output_line ch ".end method"


let emit_constructor ch cdecl =
  let csig = cdecl.LAst.constructor_signature in
  let initsig = make_sig "<init>" csig in (* chop off full sig appropriately *)
  output_newline ch;
  output_line ch (".method public " ^ initsig);
  output_line ch ".throws java/lang/Exception";
  emit_body ch cdecl.LAst.constructor_body;
  output_line ch ".end method"


let emit_method ch mdecl =
  let method_name = mdecl.LAst.method_name.Ast.identifier in
  let fullsig = mdecl.LAst.method_signature in
  let msig = make_sig method_name fullsig in (* chop off full sig appropriately *)
  output_newline ch;
  output_line ch (".method public " ^ msig);
  output_line ch ".throws java/lang/Exception";
  emit_body ch mdecl.LAst.method_body;
  output_line ch ".end method"


let emit_class ch cdecl =
  output_line ch (".class public " ^ (cdecl.LAst.class_name.Ast.identifier));
  output_line ch (".super java/lang/Object");
  List.iter (emit_field ch) cdecl.LAst.class_fields;
  emit_constructor ch cdecl.LAst.class_constructor;
  emit_main_opt ch cdecl.LAst.class_main;
  List.iter (emit_method ch) cdecl.LAst.class_methods


let emit_program prog =
  List.iter (fun cdecl ->
    let src_name  = cdecl.LAst.source_file_name in
    let file_name = (Filename.chop_extension src_name) ^ ".j" in
    try
      let ch = open_out file_name in
      output_line ch (".source " ^ (Filename.basename src_name));
      emit_class ch cdecl;
      close_out ch
    with Sys_error msg ->
      Error.error Lexing.dummy_pos ("Unable to open file " ^ msg)
  ) prog

