/* File parser.mly */

%{

open Backend;;

%}

%token <int> INT
%token <string> BINARYOP  UNARYOP
%token <float> FLOAT
%token LPAREN RPAREN LBRACKET RBRACKET COLON SEMICOLON COMMA ASSIGNMENT
%token EOF


%start parser_program            /* the entry point */
%type <unit> parser_program

%%
/* For test, returns the number of expressions is a grammatically correct program */
parser_program: expression_list               {  };

expression_list:    /*empty*/           { }
               | expr SEMICOLON expression_list {  };

expr: index ASSIGNMENT UNARYOP range        { eval_unaryR $1 $3 $4 }  /*semantic action : call eval_unaryR func */
    | index ASSIGNMENT BINARYOP range range { eval_binaryRR $1 $3 $4 $5 }  
    | index ASSIGNMENT BINARYOP FLOAT range { eval_binaryCR $1 $3 $4 $5 }  
    | index ASSIGNMENT BINARYOP range FLOAT { eval_binaryCR $1 $3 $5 $4 }  
    | index ASSIGNMENT BINARYOP range index { eval_binaryIR $1 $3 $5 $4 }
    | index ASSIGNMENT BINARYOP index range { eval_binaryIR $1 $3 $4 $5 };


range: LPAREN index COLON index RPAREN  { ($2, $4) };  /*semantic action: returns a tuple of int tuples */

index: LBRACKET INT COMMA INT RBRACKET  { ($2, $4) };  /*semantic action : return a int tuple */

%%


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

