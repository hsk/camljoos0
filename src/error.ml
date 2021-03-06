exception NotImplementedYet
exception InternalCompilerError of string
exception ErrorExit
let print_position pos =
    let line = pos.Lexing.pos_lnum in
    let cnum = pos.Lexing.pos_cnum - pos.Lexing.pos_bol in
    (* let tok = Lexing.lexeme lexbuf in*)
            (*let tail = Lexer.ruleTail "" lexbuf in*)
	    (*raise (Error (exn,(line,cnum,tok(*,tail*)))) *)
    Printf.printf " line %i, col %i" line cnum 

let print_line pos =
  let inch = open_in pos.Lexing.pos_fname in
  let rec read_line = function
    | 1 -> input_line inch;
    | n -> let _ = input_line inch in
	         read_line (n - 1)
  in
  let line = read_line pos.Lexing.pos_lnum in
  close_in inch;
  print_string line

let error pos fmt =
  let buffer = Buffer.create 16 in
  let buf = Format.formatter_of_buffer (buffer) in
  Format.kfprintf (fun ppf ->
    Format.fprintf ppf "@?";
    if pos = Lexing.dummy_pos
    then begin
      print_string "Error: ";
      print_string (Buffer.contents buffer);
      print_newline ();
      flush stdout;
      raise ErrorExit
    end;
    print_string ("Error at " ^ pos.Lexing.pos_fname ^":");
    print_position pos;
    print_newline ();
    print_line pos;
    print_newline ();
    print_newline ();
    print_string (Buffer.contents buffer);
    print_newline ();
    flush stdout;
    raise ErrorExit

  ) buf fmt
