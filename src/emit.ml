(** Final compiler phase to emit the generated code for all classes and
    interfaces to jasmin files. *)

open Limits
open Printf

let make_sig mname fullsig =
  mname ^ (Str.string_after fullsig (String.index fullsig '('))

let f_body ch {limits=(stack, locals); throws; body} = 
  List.iter(fun throw ->
    fprintf ch "  .throws java/lang/%s\n" throw;
  ) throws;
  fprintf ch "  .limit stack %d\n" stack;
  fprintf ch "  .limit locals %d\n" locals;
  List.iter (fun inst -> fprintf ch "%s\n" (Inst.to_asm inst)) body

let f_field ch = function
  | Constructor (_, body, jsig) ->
    fprintf ch "\n";
    fprintf ch ".method public %s\n" (make_sig "<init>" jsig);
    f_body ch body;
    fprintf ch ".end method\n"

  | Method(_, {Ast.id}, body, jsig) ->
    fprintf ch "\n";
    fprintf ch ".method public %s\n" (make_sig id jsig);
    f_body ch body;
    fprintf ch ".end method\n"

  | Main body ->
    fprintf ch "\n";
    fprintf ch ".method public static main([Ljava/lang/String;)V\n";
    f_body ch body;
    fprintf ch ".end method\n"

  | Field (t,{Ast.id},_) ->
    fprintf ch ".field protected %s %s\n" id (Types.t_to_sig t)

let f prog =
  List.iter (fun {cfilename; cname; cfields} ->
    let file_name = (Filename.chop_extension cfilename) ^ ".j" in
    try
      let ch = open_out file_name in
      fprintf ch ".source %s\n" (Filename.basename cfilename);
      fprintf ch ".class public %s\n" (cname.Ast.id);
      fprintf ch ".super java/lang/Object\n";

      let (fields, methods) = List.partition (function Field _ -> true | _ -> false) cfields in
      List.iter (f_field ch) fields;
      List.iter (f_field ch) methods;

      close_out ch
    with Sys_error msg ->
      Error.error Lexing.dummy_pos "Unable to open file %s" msg
  ) prog

