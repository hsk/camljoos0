%{
(** Helper function to parse an [int32] from a string.

    Int32.of_string in the standard library is buggy and will not return a 
    failure for [max_int + 1]. 

    Might be fixed in OCaml 3.12 *)
  open Ast
  let string_to_int32 s =
    let i = Int64.of_string s in
    if i < Int64.of_int32 Int32.min_int ||
      i > Int64.of_int32 Int32.max_int
    then raise (Failure "int_of_string")
    else Int64.to_int32 i

  let make_id id_pos id = { id_pos; id }
  let make_lvalue lvalue_pos lvalue = { lvalue_pos; lvalue }
  let make_t pos t = { t_pos = pos; t = t }
  let make_stm pos s = { stm_pos = pos; stm = s }
  let make_retstm pos rs = { return_stm_pos = pos; return_stm = rs }
  let make_exp e_pos exp = { e_pos; exp }


  let make_body (prms,locals,stms,return) =
    { prms; locals; stms; return }

  let make_method (texp, name, body) = Method(texp,name,body)
  let make_constructor (id, body) = Constructor(id, body)
  let make_field (texp, name, init) = Field(texp,name,init)

  let make_class (name,fields,constructor,main,methods) =
    let methods = match main with
      | None -> methods
      | Some body -> Main(body) :: methods 
    in
    fun cfilename ->
    {
      cfilename = cfilename;
      cname = name;
      cfields = fields @ constructor :: methods
    }

  let make_source_file p decl =
    decl p.Lexing.pos_fname

%}
%token EOF
(* Keywords *)
(* %token ABSTRACT *)
%token BOOLEAN
(* %token BREAK *)
(* %token BYTE *)
(* %token CASE *)
(* %token CATCH *)
%token CHAR
%token CLASS
(* %token CONST *)
(* %token CONTINUE *)
(* %token DEFAULT *)
(* %token DO *)
(* %token DOUBLE *)
%token ELSE
(* %token EXTENDS *)
(* %token FINAL *)
(* %token FINALLY *)
(* %token FLOAT *)
(* %token FOR *)
(* %token GOTO *)
%token IF
(* %token IMPLEMENTS *)
(* %token IMPORT *)
(* %token INSTANCEOF *)
%token INT
(* %token INTERFACE *)
(* %token LONG *)
(* %token NATIVE *)
%token NEW
(* %token PACKAGE *)
(* %token PRIVATE *)
%token PROTECTED
%token PUBLIC
%token RETURN
(* %token SHORT *)
%token STATIC
(* %token STRICTFP *)
(* %token SUPER *)
(* %token SWITCH *)
(* %token SYNCHRONIZED *)
%token THIS
(* %token THROW *)
%token THROWS
(* %token TRANSIENT *)
(* %token TRY *)
%token VOID
(* %token VOLATILE *)
%token WHILE

%token <string>KEYWORD

%token TRUE FALSE
%token NULL

(* Extra JOOS0 keywords *)
%token STRING
%token EXCEPTION
%token SYSTEM
%token OUT PRINT
%token IN READ
%token MAIN

(* Delimiters *)
%token L_PAREN R_PAREN
%token L_BRACE R_BRACE
%token L_BRACKET R_BRACKET
%token SEMICOLON
%token COMMA
%token DOT

(* Assignment and complement *)
%token ASSIGN
%token COMPLEMENT

(* Comparison *)
%token LT GT EQ
%token LTEQ GTEQ NEQ

(* Arithmetic *)
%token PLUS MINUS STAR DIV MOD
%token AND OR XOR

(* Literals and ids *)
%token <string>INTEGER_LITERAL
%token <string>STRING_LITERAL
%token <string>IDENTIFIER


%start <Ast.class_decl> goal          (* the entry point *)
%%

(*******************************************************************
 * Productions                                                     *
 *******************************************************************)

goal :
  | class_declaration EOF { make_source_file $startpos $1 }


  (* ********** Type declarations *********** *)

  class_declaration :
    |  PUBLIC CLASS IDENTIFIER class_body
       { let id = make_id $startpos $3 in
         let (fields,constructor,opt_main,methods) = $4 in
         make_class (id,fields,constructor,opt_main,methods) }

    class_body :
      |  L_BRACE
         fieldaration*
         constructor_declaration
         PUBLIC STATIC VOID MAIN main_method_params throws_clause
           L_BRACE NEW name L_PAREN R_PAREN SEMICOLON R_BRACE
         method_declaration*
         R_BRACE
         { let new_exp = make_exp $startpos (New ($12,[])) in (*FIXME: pos?*)
           let new_stm = make_stm $startpos (Exp new_exp) in (*FIXME: pos?*)
           let new_retstm = make_retstm $startpos VoidReturn in(*FIXME: pos?*)
           let body = make_body ([],[],[new_stm],new_retstm) in
           ($2,$3,Some body,$17) }
      |  L_BRACE
         fieldaration*
         constructor_declaration (* no main decl *)
         method_declaration*
         R_BRACE
         { ($2,$3,None,$4) }

      (* ********** Field declarations ********** *)

      fieldaration :
        |  PROTECTED t IDENTIFIER variable_initializer? SEMICOLON
           { let id = make_id $startpos $3 in
             make_field ($2,id,$4) }

      variable_initializer :
        |  ASSIGN exp { $2 }

      (* ********** Method declarations ********** *)

      main_method_params :
        |  L_PAREN STRING L_BRACKET R_BRACKET IDENTIFIER R_PAREN { make_id $startpos $5 }

      method_declaration :
        |  PUBLIC t_or_void IDENTIFIER method_params throws_clause method_body
           { let id = make_id $startpos $3 in 
             let (lvars,stms,retstm) = $6 in
             let body = make_body ($4,lvars,stms,retstm) in
             make_method ($2,id,body) }

        t_or_void :
          |  VOID { make_t $startpos Void }
          |  t { $1 }

        method_params :
          |  L_PAREN formal_parameter_list R_PAREN { $2 }

          formal_parameter_list :
            |  { [] }
            |  formal_parameter_list_nonempty { $1 }

            formal_parameter_list_nonempty :
              |  formal_parameter { [$1] }
              |  formal_parameter_list_nonempty COMMA formal_parameter { $1 @ [$3] }

              formal_parameter :
                |  t IDENTIFIER { ($1,make_id $startpos $2) }

        method_body :
          |  L_BRACE local_variable_declarations statement* return_statement R_BRACE { ($2,$3,$4) }

      throws_clause :
        |  THROWS EXCEPTION { () }

      (* ********** Constructor declarations ********** *)

      constructor_declaration :
        |  PUBLIC constructor_declarator throws_clause constructor_body
           { let (id,prms) = $2 in 
             let (lvars,stms) = $4 in
             let retstm       = make_retstm $startpos VoidReturn in
             let body = make_body (prms,lvars,stms,retstm) in
             make_constructor (id,body) }

        constructor_declarator  :
          |  IDENTIFIER L_PAREN formal_parameter_list R_PAREN
             { let id = make_id $startpos $1 in
               (id,$3) }

        constructor_body :
          |  L_BRACE local_variable_declarations statement* void_return_statement? R_BRACE
             { ($2,$3) }


      (* ********** Types ********** *)

      t :
        |  primitive_type { $1 }
        |  reference_type { $1 }

        primitive_type :
          |  BOOLEAN { make_t $startpos Boolean }
          |  INT { make_t $startpos Int }

        reference_type :
          |  name { make_t $startpos (Class $1) }
          |  STRING { make_t $startpos String }

      (* ********** Blocks and stms ********** *)

      local_variable_declarations :
        | { [] }
        |  local_variable_declarations local_variable_declaration { $1 @ [$2] }

        local_variable_declaration :
          |  t IDENTIFIER variable_initializer SEMICOLON { ($1,make_id $startpos $2,$3) }

      block :
        |  L_BRACE statement* R_BRACE { $2 }

      statement :
        |  statement_without_trailing_substatement { $1 }
        |  if_then_statement { $1 }
        |  if_then_else_statement { $1 }
        |  while_statement { $1 }

        if_then_statement :
          |  IF L_PAREN exp R_PAREN statement { make_stm $startpos (IfThen($3,$5)) }

        if_then_else_statement :
          |  IF L_PAREN exp R_PAREN statement_no_short_if ELSE statement { make_stm $startpos (IfThenElse($3,$5,$7)) }

        while_statement :
          |  WHILE L_PAREN exp R_PAREN statement { make_stm $startpos (While($3,$5)) }

      statement_no_short_if :
        |  statement_without_trailing_substatement { $1 }
        |  if_then_else_statement_no_short_if { $1 }
        |  while_statement_no_short_if { $1 }

        if_then_else_statement_no_short_if :
          |  IF L_PAREN exp R_PAREN statement_no_short_if ELSE statement_no_short_if { make_stm $startpos (IfThenElse($3,$5,$7)) }

        while_statement_no_short_if :
          |  WHILE L_PAREN exp R_PAREN statement_no_short_if { make_stm $startpos (While($3,$5)) }

      statement_without_trailing_substatement :
        |  block { make_stm $startpos (Block $1) }
        |  empty_statement { make_stm $startpos (Empty) }
        |  exp_statement { $1 }

        empty_statement :
          |  SEMICOLON { make_stm $startpos (Empty) }

        exp_statement :
          |  statement_exp SEMICOLON { make_stm $startpos (Exp $1) }


          statement_exp :
            |  assign { $1 }
            |  method_invocation { $1 }
            |  class_instance_creation_exp { $1 }

      return_statement :
        |  void_return_statement { $1 }
        |  value_return_statement { $1 }

        value_return_statement :
          |  RETURN exp SEMICOLON { make_retstm $startpos (ValueReturn $2) }

      void_return_statement :
        |  RETURN SEMICOLON { make_retstm $startpos VoidReturn }



      (* ********** Literals and names ********** *)

      literal_not_integer :
        |  MINUS INTEGER_LITERAL
            { try
                let i = string_to_int32 ("-" ^ $2) in
                make_exp $startpos (IntConst i)
              with Failure msg -> 
                Error.error $startpos "Integer value out of range: %s" msg }
        |  boolean_literal { make_exp $startpos (BooleanConst $1) }
        |  STRING_LITERAL { make_exp $startpos (StringConst $1) }
        |  null_literal { make_exp $startpos (Null) }

      boolean_literal :
        |  TRUE { true }
        |  FALSE { false }

      null_literal :
        |  NULL { () }

      name :
        |  IDENTIFIER { make_id $startpos $1 }


      (* ********** Expressions ********** *)

      primary :
        |  literal { $1 }
        |  THIS { make_exp $startpos (This) }
        |  left_hand_side { make_exp $startpos (Lvalue $1) }
        |  L_PAREN exp R_PAREN { $2 }
        |  class_instance_creation_exp { $1 }
        |  method_invocation { $1 }

        literal :
          |  INTEGER_LITERAL
              { try
                  let i = string_to_int32 $1 in
                  make_exp $startpos (IntConst i)
                with Failure msg -> 
                  Error.error $startpos "Integer value out of range: %s" msg }
          |  MINUS INTEGER_LITERAL
              { try
                  let i = string_to_int32 ("-" ^ $2) in
                  make_exp $startpos (IntConst i)
                with Failure msg -> 
                  Error.error $startpos "Integer value out of range: %s" msg }
          |  boolean_literal { make_exp $startpos (BooleanConst $1) }
          |  STRING_LITERAL { make_exp $startpos (StringConst $1) }
          |  null_literal { make_exp $startpos (Null) }

      class_instance_creation_exp :
        |  NEW name L_PAREN argument_list R_PAREN { make_exp $startpos (New($2,$4)) }

      argument_list :
        |  { [] }
        |  argument_list_nonempty { $1 }

        argument_list_nonempty :
          |  exp { [$1] }
          |  argument_list_nonempty COMMA exp { $1 @ [$3] }

      method_invocation :
        |  THIS DOT IDENTIFIER L_PAREN argument_list R_PAREN
           { let this = make_exp $startpos This in
             let id = make_id $startpos $3 in
             make_exp $startpos (Invoke(this,id,$5)) }
        |  primary_not_integer_not_this DOT IDENTIFIER L_PAREN argument_list R_PAREN
           { let id = make_id $startpos $3 in
             make_exp $startpos (Invoke($1,id,$5)) }
        |  SYSTEM DOT OUT DOT PRINT L_PAREN exp R_PAREN { make_exp $startpos (Print $7) }
        |  SYSTEM DOT IN DOT READ L_PAREN R_PAREN { make_exp $startpos Read }

        primary_not_integer_not_this :
          |  literal_not_integer { $1 }
          |  left_hand_side { make_exp $startpos (Lvalue $1) }
          |  L_PAREN exp R_PAREN { $2 }
          |  class_instance_creation_exp { $1 }
          |  method_invocation { $1 }

      exp :
        |  inclusive_or_exp { $1 }
        |  assign { $1 }

        inclusive_or_exp :
          |  xor_exp { $1 }
          |  inclusive_or_exp OR xor_exp { make_exp $startpos (Binop($1,Or,$3)) }

          xor_exp :
            |  and_exp { $1 }
            |  xor_exp XOR and_exp { make_exp $startpos (Binop($1,Xor,$3)) }

            and_exp :
              |  equ_exp { $1 }
              |  and_exp AND equ_exp { make_exp $startpos (Binop($1,And,$3)) }

              equ_exp :
                |  rel_exp { $1 }
                |  equ_exp EQ rel_exp { make_exp $startpos (Binop($1,Eq,$3)) }
                |  equ_exp NEQ rel_exp { make_exp $startpos (Binop($1,Ne,$3)) }

                rel_exp :
                  |  add_exp { $1 }
                  |  rel_exp LT add_exp { make_exp $startpos (Binop($1,Lt,$3)) }
                  |  rel_exp GT add_exp { make_exp $startpos (Binop($1,Gt,$3)) }
                  |  rel_exp LTEQ add_exp { make_exp $startpos (Binop($1,Le,$3)) }
                  |  rel_exp GTEQ add_exp { make_exp $startpos (Binop($1,Ge,$3)) }

                  add_exp :
                    |  mul_exp { $1 }
                    |  add_exp PLUS mul_exp { make_exp $startpos (Binop($1,Add,$3)) }
                    |  add_exp PLUS L_PAREN CHAR R_PAREN unary_exp
                       { let unexp = make_exp $startpos (Unop(CharToString,$6)) in
                         make_exp $startpos (Binop($1,Concat,unexp)) }
                    |  add_exp MINUS mul_exp
                       { make_exp $startpos (Binop($1,Minus,$3)) }

                    mul_exp :
                      |  unary_exp { $1 }
                      |  mul_exp STAR unary_exp { make_exp $startpos (Binop($1,Times,$3)) }
                      |  mul_exp DIV unary_exp { make_exp $startpos (Binop($1,Divide,$3)) }
                      |  mul_exp MOD unary_exp { make_exp $startpos (Binop($1,Modulo,$3)) }

                      unary_exp :
                        |  primary { $1 }
                        |  MINUS unary_exp_not_integer { make_exp $startpos (Unop(Negate,$2)) }
                        |  COMPLEMENT unary_exp { make_exp $startpos (Unop(Complement,$2)) }

                        unary_exp_not_integer :
                          |  primary_not_integer { $1 }
                          |  MINUS unary_exp_not_integer { make_exp $startpos (Unop(Negate,$2)) }
                          |  COMPLEMENT unary_exp { make_exp $startpos (Unop(Complement,$2)) }

                          primary_not_integer :
                            |  literal_not_integer { $1 }
                            |  THIS { make_exp $startpos (This) }
                            |  left_hand_side { make_exp $startpos (Lvalue $1) }
                            |  L_PAREN exp R_PAREN { $2 }
                            |  class_instance_creation_exp { $1 }
                            |  method_invocation { $1 }

      assign :
        |  left_hand_side ASSIGN exp { make_exp $startpos (Assignment($1,$3)) }

      left_hand_side :
        |  name { make_lvalue $startpos (Local $1) }
        |  THIS DOT name { make_lvalue $startpos (Field $3) }
