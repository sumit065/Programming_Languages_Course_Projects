type token =
  | INT of (int)
  | BINARYOP of (string)
  | UNARYOP of (string)
  | FLOAT of (float)
  | LPAREN
  | RPAREN
  | LBRACKET
  | RBRACKET
  | COLON
  | SEMICOLON
  | COMMA
  | ASSIGNMENT
  | EOF

open Parsing;;
let _ = parse_error;;
# 4 "parser.mly"

open Backend;;

# 23 "parser.ml"
let yytransl_const = [|
  261 (* LPAREN *);
  262 (* RPAREN *);
  263 (* LBRACKET *);
  264 (* RBRACKET *);
  265 (* COLON *);
  266 (* SEMICOLON *);
  267 (* COMMA *);
  268 (* ASSIGNMENT *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  257 (* INT *);
  258 (* BINARYOP *);
  259 (* UNARYOP *);
  260 (* FLOAT *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\003\000\003\000\003\000\003\000\003\000\
\003\000\005\000\004\000\000\000"

let yylen = "\002\000\
\001\000\000\000\003\000\004\000\005\000\005\000\005\000\005\000\
\005\000\005\000\005\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\012\000\001\000\000\000\000\000\000\000\
\000\000\000\000\000\000\003\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\004\000\011\000\006\000\000\000\009\000\
\007\000\008\000\005\000\000\000\000\000\010\000"

let yydgoto = "\002\000\
\004\000\005\000\006\000\007\000\019\000"

let yysindex = "\010\000\
\010\255\000\000\017\255\000\000\000\000\011\255\007\255\012\255\
\010\255\255\254\019\255\000\000\005\255\020\255\014\255\020\255\
\010\255\020\255\009\255\000\000\000\000\000\000\015\255\000\000\
\000\000\000\000\000\000\010\255\021\255\000\000"

let yyrindex = "\000\000\
\026\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\026\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\019\000\000\000\243\255\245\255"

let yytablesize = 28
let yytable = "\018\000\
\013\000\014\000\020\000\023\000\022\000\026\000\024\000\027\000\
\016\000\017\000\001\000\003\000\025\000\017\000\029\000\003\000\
\003\000\008\000\010\000\015\000\009\000\021\000\011\000\028\000\
\017\000\002\000\030\000\012\000"

let yycheck = "\013\000\
\002\001\003\001\014\000\017\000\016\000\019\000\018\000\019\000\
\004\001\005\001\001\000\007\001\004\001\005\001\028\000\007\001\
\007\001\001\001\012\001\001\001\010\001\008\001\011\001\009\001\
\005\001\000\000\006\001\009\000"

let yynames_const = "\
  LPAREN\000\
  RPAREN\000\
  LBRACKET\000\
  RBRACKET\000\
  COLON\000\
  SEMICOLON\000\
  COMMA\000\
  ASSIGNMENT\000\
  EOF\000\
  "

let yynames_block = "\
  INT\000\
  BINARYOP\000\
  UNARYOP\000\
  FLOAT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expression_list) in
    Obj.repr(
# 21 "parser.mly"
                                              (  )
# 114 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    Obj.repr(
# 23 "parser.mly"
                                        ( )
# 120 "parser.ml"
               : 'expression_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression_list) in
    Obj.repr(
# 24 "parser.mly"
                                                (  )
# 128 "parser.ml"
               : 'expression_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'index) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'range) in
    Obj.repr(
# 26 "parser.mly"
                                            ( eval_unaryR _1 _3 _4 )
# 137 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'index) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'range) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'range) in
    Obj.repr(
# 27 "parser.mly"
                                            ( eval_binaryRR _1 _3 _4 _5 )
# 147 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'index) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : float) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'range) in
    Obj.repr(
# 28 "parser.mly"
                                            ( eval_binaryCR _1 _3 _4 _5 )
# 157 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'index) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'range) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 29 "parser.mly"
                                            ( eval_binaryCR _1 _3 _5 _4 )
# 167 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'index) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'range) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'index) in
    Obj.repr(
# 30 "parser.mly"
                                            ( eval_binaryIR _1 _3 _5 _4 )
# 177 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'index) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'index) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'range) in
    Obj.repr(
# 31 "parser.mly"
                                            ( eval_binaryIR _1 _3 _4 _5 )
# 187 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'index) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'index) in
    Obj.repr(
# 34 "parser.mly"
                                        ( (_2, _4) )
# 195 "parser.ml"
               : 'range))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : int) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 36 "parser.mly"
                                        ( (_2, _4) )
# 203 "parser.ml"
               : 'index))
(* Entry parser_program *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let parser_program (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : unit)
;;
# 39 "parser.mly"


let read_tokens token_filename = 
    let fin = open_in token_filename in
    let tokens_queue = Queue.create () in   
    let get_line () = String.trim (input_line fin) in
    (try 
        while true do
            let token_type = get_line () in
            let token = match token_type with
                | "INT" -> INT(int_of_string (get_line ()))
                | "FLOAT" -> FLOAT(float_of_string (get_line ()))
                | "BINARYOP" -> BINARYOP(get_line ())
                | "UNARYOP" -> UNARYOP(get_line ())

                | "ASSIGNMENT" -> ASSIGNMENT
                | "SEMICOLON" -> SEMICOLON
                | "LPAREN" -> LPAREN
                | "RPAREN" -> RPAREN
                | "LBRACKET" -> LBRACKET
                | "RBRACKET" -> RBRACKET
                | "COLON" -> COLON
                | "COMMA" -> COMMA

                | _ -> begin
                    Printf.printf "??? Unexpected Token: %s \n" token_type;
                    exit 1 
                end in
                Queue.add token tokens_queue              
        done
    with _ -> ());
    close_in fin;
    tokens_queue


let main () = begin 
    let token_filename = Sys.argv.(1) in
    let tokens_queue = read_tokens token_filename in
    let lexbuf = Lexing.from_string "" in
    let lexer_token lb = 
        if Queue.is_empty tokens_queue then
            EOF
        else begin
            let next_token = Queue.take tokens_queue in
            next_token
        end
    in
    let _ = parser_program lexer_token lexbuf in
    Printf.printf "Parsed Successfully\n";
end;;

read_csv Sys.argv.(2);;
Printf.printf "Sheet Read Successfully!\nStarting Computation.....\n";;

main () ;;

write_to_csv Sys.argv.(2);;
Printf.printf "Sheet Updated Successfully\n";;

# 288 "parser.ml"
