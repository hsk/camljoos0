(** A representation of JVM bytecode instructions along with operations.
    Some instruction classes correspond to one of a number of different actual JVM instructions, 
    depending on the  values of its parameters.  *)

type label = string

(** Returns a fresh label with a given string prefix. *)
let make_label =
  let count = ref 0 in
  fun prefix ->
    let label = prefix ^ (string_of_int !count) in
    count := !count + 1;
    label


type signature = string

type method_signature = { method_sig : signature;
                          method_nargs : int;
                          method_nreturns : int }

type condition =
  | Eq
  | Ne
  | Lt
  | Le
  | Gt
  | Ge
  | Aeq
  | Ane

(**  The condition equivalent to negating the result of this condition. *)
let negate = function
  | Eq  -> Ne
  | Ne  -> Eq
  | Lt  -> Ge
  | Le  -> Gt
  | Gt  -> Le
  | Ge  -> Lt
  | Aeq -> Ane
  | Ane -> Aeq

(** The condition equivalent to switching the arguments of this condition. *)
let commute = function
  | Eq  -> Eq
  | Ne  -> Ne
  | Lt  -> Gt
  | Le  -> Ge
  | Gt  -> Lt
  | Ge  -> Le
  | Aeq -> Aeq
  | Ane -> Ane

(**/**) (*/*)

(** The [if] instruction comparing a value to zero or null using the given condition. *)
let if_name = function
  | Eq  -> "ifeq"
  | Ne  -> "ifne"
  | Lt  -> "iflt"
  | Le  -> "ifle"
  | Gt  -> "ifgt"
  | Ge  -> "ifge"
  | Aeq -> "ifnull"
  | Ane -> "ifnonnull"

(** The [if] instruction comparing two values using the given condition. *)
let ifcmp_name = function
  | Eq  -> "if_icmpeq"
  | Ne  -> "if_icmpne"
  | Lt  -> "if_icmplt"
  | Le  -> "if_icmple"
  | Gt  -> "if_icmpgt"
  | Ge  -> "if_icmpge"
  | Aeq -> "if_acmpeq"
  | Ane -> "if_acmpne"

(**/**) (*/*)

(** The datatype of bytecode instructions  *)
type instruction =
  | Iaaload
  | Iaastore
  | Iaconst_null
  | Iaload of int
  | Iareturn
  | Iarraylength
  | Iastore of int
  | Iathrow
  | Ibaload
  | Ibastore
  | Icaload
  | Icastore
  | Icheckcast of signature
  | Idup
  | Idup2
  | Idup2_x1
  | Idup2_x2
  | Idup_x1
  | Idup_x2
  | Igetfield of signature
  | Igetstatic of signature
  | Igoto of label
  | Ii2b
  | Ii2c
  | Ii2s
  | Iiadd
  | Iiaload
  | Iiand
  | Iiastore
  | Iidiv
  | Iif of condition * label
  | Iifcmp of condition * label
  | Iiinc of int * int32
  | Iiload of int
  | Iimul
  | Iineg
  | Iinstanceof of signature
  | Iinvokeinterface of method_signature
  | Iinvokespecial of method_signature
  | Iinvokestatic of method_signature
  | Iinvokevirtual of method_signature
  | Iior
  | Iirem
  | Iireturn
  | Iistore of int
  | Iisub
  | Iixor
  | Ilabel of label
  | Ildc_int of int32
  | Ildc_string of string
  | Imultianewarray of signature * int
  | Inew of signature
  | Inop
  | Ipop
  | Ipop2
  | Iputfield of signature
  | Iputstatic of signature
  | Ireturn
  | Isaload
  | Isastore
  | Iswap

(**/**) (*/*)

(*  ldc_int_inst int32 -> string  *)
let ldc_int_inst i =
    if i = -1l then "iconst_m1" else
    let istr = Int32.to_string i in
    if 0l <= i && i <= 5l then "iconst_" ^ istr else
    if -128l <= i && i <= 127l then "bipush " ^ istr else
    if -32768l <=i && i <= 32767l then "sipush " ^ istr else
    "ldc " ^ istr

(*  escape : string -> string  *)
let escape s =
  let buf = Buffer.create (String.length s) in
  String.iter (fun c ->
    let ccode = int_of_char c in
    if ccode < 32 || 126 < ccode then
    begin
      Buffer.add_string buf "\\";
      Buffer.add_string buf (string_of_int (ccode / 64));
      Buffer.add_string buf (string_of_int ((ccode / 8) land 7));
      Buffer.add_string buf (string_of_int (ccode land 7))
    end else
    if c = '\"' then Buffer.add_string buf "\\\"" else
    if c = '\\' then Buffer.add_string buf "\\\\" else
    Buffer.add_char buf c
  ) s;
  Buffer.contents buf

(**/**) (*/*)

(** Returns the instruction in [jasmin] format.
     @return the instruction as a string 
*)
let to_asm = function
  | Iaaload -> "aaload"
  | Iaastore -> "aastore"
  | Iaconst_null -> "aconst_null"
  | Iaload v -> if v <= 3 
                then "aload_" ^ (string_of_int v)
                else "aload " ^ (string_of_int v)
  | Iareturn -> "areturn"
  | Iarraylength -> "arraylength"
  | Iastore v -> if v <= 3 
                 then "astore_" ^ (string_of_int v)
                 else "astore " ^ (string_of_int v)
  | Iathrow -> "athrow"
  | Ibaload -> "baload"
  | Ibastore -> "bastore"
  | Icaload -> "caload"
  | Icastore -> "castore"
  | Icheckcast s -> "checkcast " ^ s
  | Idup -> "dup"
  | Idup2 -> "dup2"
  | Idup2_x1 -> "dup2_x1"
  | Idup2_x2 -> "dup2_x2"
  | Idup_x1 -> "dup_x1"
  | Idup_x2 -> "dup_x2"
  | Igetfield s -> "getfield " ^ s
  | Igetstatic s -> "getstatic " ^ s
  | Igoto l -> "goto " ^ l
  | Ii2b -> "i2b"
  | Ii2c -> "i2c"
  | Ii2s -> "i2s"
  | Iiadd -> "iadd"
  | Iiaload -> "iaload"
  | Iiand -> "iand"
  | Iiastore -> "iastore"
  | Iidiv -> "idiv"
  | Iif (c,tgt) -> (if_name c) ^ " " ^ tgt
  | Iifcmp (c,tgt) -> (ifcmp_name c) ^ " " ^ tgt
  | Iiinc (v,i) -> "iinc " ^ (string_of_int v) ^ " " ^ (Int32.to_string i)
  | Iiload v -> if v <= 3 
                then "iload_" ^ (string_of_int v)
                else "iload " ^ (string_of_int v)
  | Iimul -> "imul"
  | Iineg -> "ineg"
  | Iinstanceof s -> "instanceof " ^ s
  | Iinvokeinterface m -> "invokeinterface " ^ m.method_sig ^ " " ^
                                 (string_of_int (1 + m.method_nargs))
  | Iinvokespecial m -> "invokespecial " ^ m.method_sig
  | Iinvokestatic m -> "invokestatic " ^ m.method_sig
  | Iinvokevirtual m -> "invokevirtual " ^ m.method_sig
  | Iior -> "ior"
  | Iirem -> "irem"
  | Iireturn -> "ireturn"
  | Iistore v -> if v <= 3 
                 then "istore_" ^ (string_of_int v)
                 else "istore " ^ (string_of_int v)
  | Iisub -> "isub"
  | Iixor -> "ixor"
  | Ilabel l -> l ^ ":"
  | Ildc_int i -> ldc_int_inst i
  | Ildc_string s -> "ldc \"" ^ (escape s) ^ "\""
  | Imultianewarray (s,dims) -> "multianewarray " ^ s ^ " " ^ (string_of_int dims)
  | Inew s -> "new " ^ s
  | Inop -> "nop"
  | Ipop -> "pop"
  | Ipop2 -> "pop2"
  | Iputfield s -> "putfield " ^ s
  | Iputstatic s -> "putstatic " ^ s
  | Ireturn -> "return"
  | Isaload -> "saload"
  | Isastore -> "sastore"
  | Iswap -> "swap"

(** Returns the number of stack slots that the stack height will change when
    this instruction is executed. A positive value means the stack will grow.
    A negative value means the stack will shrink.
     @return the stack change
*)
let stack_change = function
  | Iaastore
  | Ibastore | Icastore
  | Iiastore | Isastore -> -3

  | Iifcmp _ | Ipop2 | Iputfield  _ -> -2

  | Ipop
  | Iputstatic _
  | Iathrow
  | Iif _
  | Iireturn  | Iareturn
  | Iistore _ | Iastore _
  | Iiadd | Iisub
  | Iimul | Iidiv
  | Iiand | Iior
  | Iirem | Iixor
  | Iaaload
  | Ibaload | Icaload 
  | Iiaload | Isaload -> -1

  | Iarraylength
  | Icheckcast _
  | Igetfield _
  | Igoto _
  | Ii2b | Ii2c | Ii2s
  | Inop
  | Iiinc _ | Iineg
  | Iinstanceof _
  | Ilabel _
  | Ireturn
  | Iswap         -> 0

  | Ildc_int _ | Ildc_string _
  | Inew _     | Iaconst_null
  | Iaload _   | Iiload _      | Igetstatic _
  | Idup  | Idup_x1  | Idup_x2  -> 1

  | Idup2 | Idup2_x1 | Idup2_x2 -> 2


  | Iinvokeinterface m -> -1 - m.method_nargs + m.method_nreturns
  | Iinvokespecial   m -> -1 - m.method_nargs + m.method_nreturns
  | Iinvokestatic    m ->    - m.method_nargs + m.method_nreturns
  | Iinvokevirtual   m -> -1 - m.method_nargs + m.method_nreturns
  | Imultianewarray (_,dims) -> -dims + 1


(** Does this instruction access a local variable?
     @return [Some v] if [v] is the highest local variable index accessed, or
             [None] if no local variables are accessed.
*)
let local_access = function
  | Iaload v    -> Some v
  | Iastore v   -> Some v
  | Iiinc (v,_) -> Some v
  | Iiload v    -> Some v
  | Iistore v   -> Some v
  | _           -> None

(** Can this instruction transfer control to a label?
     @return [Some l] if the instruction can jump to label [l], or
             [None] if the instruction cannot jump
*)
let can_jump = function
  | Igoto l      -> Some l
  | Iif (_,l)    -> Some l
  | Iifcmp (_,l) -> Some l
  | _            -> None

(** Can this instruction transfer control to the succeeding instruction?
     @return whether or not the instruction can fall through
*)
let can_fall_through = function
  | Iareturn -> false
  | Iathrow  -> false
  | Igoto _  -> false
  | Iireturn -> false
  | Ireturn  -> false
  | _        -> true

