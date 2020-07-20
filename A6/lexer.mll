(* File lexer.mll *)
{
type token = 
    | VAR of string
    | ID of string
    | LPAREN
    | RPAREN
    | PERIOD
    | COMMA
    | IFF

exception EOF
}


rule token = parse
    [' ' '\t' '\n']     { token lexbuf }     (* skip blanks *)
  | ['.' ]        { PERIOD }
  | [',' ]        { COMMA }
  | '('                                                              { LPAREN                            }
  | ')'                                                              { RPAREN                             }
  | ":-"                                                             { IFF                            }
  | ['A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_' ''']*    as lxm         { VAR (lxm)                      }
  | ['a'-'z']['a'-'z' 'A'-'Z' '0'-'9' '_' ''']*    as lxm         { ID (lxm)                      }
  | eof {raise EOF}

{
let main () = begin
    let outbuf = Buffer.create 255 in
    try
        let filename = Sys.argv.(1) in
        let file_handle = open_in filename in
        let lexbuf = Lexing.from_channel file_handle in
        while true do
            let result = token lexbuf in
            match result with
                | VAR(x) -> Printf.bprintf outbuf "VAR \n%s \n" x
                | ID(x) -> Printf.bprintf outbuf "ID \n%s \n" x
                | IFF  -> Printf.bprintf outbuf "IFF \n"
                | PERIOD -> Printf.bprintf outbuf "PERIOD \n"
                | LPAREN -> Printf.bprintf outbuf "LPAREN \n"
                | RPAREN -> Printf.bprintf outbuf "RPAREN \n"
                | COMMA -> Printf.bprintf outbuf "COMMA \n"

        done
    with |  EOF -> begin
            let filename = Sys.argv.(1) ^ "-lex" in
            let file_handle = open_out filename in
            Printf.fprintf file_handle "%s" (Buffer.contents outbuf); 
            close_out file_handle
         end
         | _ -> begin
            Printf.printf "Error while printing to file";
            exit 1
         end   
end;;
main () ;;
}