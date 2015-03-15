
(** Parse a java file *)
let parse_file file_name =
  try 
    let inch = open_in file_name in
    let () = print_endline ("Opening \"" ^ file_name ^ "\"") in
    let lexbuf = Lexing.from_channel inch in
    let lcp = lexbuf.Lexing.lex_curr_p in
    let () = lexbuf.Lexing.lex_curr_p <- { lcp with Lexing.pos_fname = file_name } in
    try
      let sf = Parser.goal Lexer.token lexbuf in
      close_in inch;
      flush stdout;
      sf
    with
    | End_of_file
    | Parser.Error ->
      let curr_pos = lexbuf.Lexing.lex_curr_p in
      close_in inch;
      Error.error curr_pos "Parse error"
    | Failure msg ->
      let curr_pos = lexbuf.Lexing.lex_curr_p in
      close_in inch;
      Error.error curr_pos "%s" msg
  with
    | End_of_file ->
      Error.error Lexing.dummy_pos "Parse error"
    | Sys_error msg ->
      Error.error Lexing.dummy_pos "Unable to open file %s" msg

let compile filenames =
  begin try
    let apply phase ast msg =
      print_string "*** ";
      print_endline msg;
      let ast = phase ast in 
      flush stdout;
      ast
    in
    let () = print_endline "Applying phases:" in
    let prog = apply (List.map parse_file) filenames "parsing" in 
    let tenv = apply Env.f            prog "environment building" in
    let last = apply (Link.f tenv)    prog "linking/name resolving" in
    let tast = apply (Typing.f tenv)  last "type checking" in
    let cast = apply Constfold.f      tast "constant folding" in
    let rast = apply Res.f            cast "resource analyzing" in
    let cast = apply (Codegen.f tenv) rast "code generation" in
    let last = apply Limits.f         cast "limit analyzing and verification" in
    let ()   = apply Emit.f           last "code emitting" in
    0
  with
    | Error.ErrorExit -> 1
  end

let _ =
  let filenames = ref [] in
  let argspec = Arg.align [] in
  let usagemsg = "Usage: joos0 <filenames>" in
  Arg.parse argspec (fun s -> filenames := s::(!filenames)) usagemsg;
  if !Sys.interactive then
    begin
      (* We are in the interactive toplevel *)
      print_newline ();
      print_endline "Welcome to the dOvs CamlJoos 0 Compiler, version 2015.\n";
      print_endline "To parse a file use:";
      print_endline "  # Main.parse_file \"myfile.java\";;\n";
      print_endline "To compile a file use:";
      print_endline "  # Main.compile [\"myfile1.java\"; \"myfile2.java\"];;\n";
      print_endline "You can also access other modules or open a module:";
      print_endline "  # open Globals;;";
      print_endline "  # !joos1;;";
      print_endline "  - : bool = false\n";
      print_endline "Happy hacking!\n"
    end else
  (* We are in the batch compiler *)
  print_endline "The CamlJoos 0 Compiler, version 2015";
  print_newline ();
  if !filenames = [] then
    (print_endline ("Error: No filename(s) provided"); Arg.usage argspec usagemsg) else
  exit (compile (List.rev !filenames))
