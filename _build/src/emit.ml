(** Final compiler phase to emit the generated code for all classes and
    interfaces to jasmin files. *)

open Limits

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


let emit_field ch (field_type,{Ast.identifier},_) =
  let field_type = Types.typeexp_to_sig field_type in
  output_line ch (".field protected " ^ identifier ^ " " ^ field_type)


let emit_body ch {max_stack;max_locals;body} = 
  output_line ch (".limit stack " ^ (string_of_int max_stack));
  output_line ch (".limit locals " ^ (string_of_int max_locals));
  List.iter (fun inst -> output_line ch (Inst.to_asm inst)) body


let emit_method ch = function
  | Constructor (name, body, signature) ->
    let initsig = make_sig "<init>" signature in (* chop off full sig appropriately *)
    output_newline ch;
    output_line ch (".method public " ^ initsig);
    output_line ch ".throws java/lang/Exception";
    emit_body ch body;
    output_line ch ".end method"

  | Method(_, name, body, signature) ->
    let name = name.Ast.identifier in
    let msig = make_sig name signature in (* chop off full sig appropriately *)
    output_newline ch;
    output_line ch (".method public " ^ msig);
    output_line ch ".throws java/lang/Exception";
    emit_body ch body;
    output_line ch ".end method"

  | Main body ->
      output_newline ch;
      output_line ch ".method public static main([Ljava/lang/String;)V";
      output_line ch ".throws java/lang/Exception";
      emit_body ch body;
      output_line ch ".end method"


let emit_program prog =
  List.iter (fun ({source_file_name;class_name;class_fields;class_methods}) ->
    let file_name = (Filename.chop_extension source_file_name) ^ ".j" in
    try
      let ch = open_out file_name in
      output_line ch (".source " ^ (Filename.basename source_file_name));
      output_line ch (".class public " ^ (class_name.Ast.identifier));
      output_line ch (".super java/lang/Object");

      List.iter (emit_field ch) class_fields;
      List.iter (emit_method ch) class_methods;

      close_out ch
    with Sys_error msg ->
      Error.error Lexing.dummy_pos ("Unable to open file " ^ msg)
  ) prog

