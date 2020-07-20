/* File parser.mly */

%{

open Backend;;

%}

%token <string> ID VAR
%token LPAREN RPAREN COMMA IFF PERIOD
%token EOF


%start parser_program            /* the entry point */
%type <Backend.clause list> parser_program

%%
/* For test, returns the number of expressions is a grammatically correct program */
parser_program: clause_list               { $1 };

clause_list:    /*empty*/           { [] }
               | clause PERIOD clause_list { $1::$3  };

clause: term     { Clause ($1, [])}  /*semantic action : ??? */
       | term IFF term_list { Clause ($1 , $3)} ;
    
term_list: term       { [$1]  }
         | term COMMA term_list  { ($1)::($3) };

term: ID LPAREN symbol_list RPAREN  { Term (Id $1, $3) };

symbol_list : symbol   { [$1] }
            | symbol COMMA symbol_list { ($1)::($3) };

symbol : ID  {Id $1 }
        | VAR   { Var $1 }
        | term { $1 } ;

%%


let read_tokens token_filename = 
    let fin = open_in token_filename in
    let tokens_queue = Queue.create () in   
    let get_line () = String.trim (input_line fin) in
    (try 
        while true do
            let token_type = get_line () in
            let token = match token_type with
                | "ID" -> ID(get_line ())
                | "VAR" -> VAR(get_line ())

                | "LPAREN" -> LPAREN
                | "RPAREN" -> RPAREN
                | "COMMA" -> COMMA
                | "IFF" -> IFF
                | "PERIOD" -> PERIOD

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
    (*let query = Stdin something in    ;;and pass it to the fucc below*)
    let res = parser_program lexer_token lexbuf in
    print_list res query [] 0 [];  (*pass query here*)
    Printf.printf "Parsed Successfully\n";
end;;

main () ;;

(* write_to_csv Sys.argv.(2);;
Printf.printf "Sheet Updated Successfully\n";; *)