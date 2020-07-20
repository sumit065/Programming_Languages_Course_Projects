{
type token = 
    | INT of int 
    | FLOAT of float 
    | BINARYOP of string
    | UNARYOP of string
    | ASSIGNMENT
    | SEMICOLON
    | COLON
    | COMMA
    | LBRACKET
    | RBRACKET
    | LPAREN
    | RPAREN

exception EOF
}

rule token = parse
    [' ' '\t' '\n']                                              {token lexbuf }     (* skip blanks *)
    | ['0'-'9']+ as lxm                                     {INT(int_of_string lxm) }
    | ("+"|"-")*(['0'-'9']*['.'])?['0'-'9']+ as lxm         {FLOAT(float_of_string lxm) } 
    | ":="                                                  {ASSIGNMENT}
    | ";"                                                   {SEMICOLON}
    | "["                                                   {LBRACKET}
    | "]"                                                   {RBRACKET}
    | ":"                                                   {COLON}
    | ","                                                   {COMMA}
    | "("                                                   {LPAREN}
    | ")"                                                   {RPAREN}

    |("ADD"
    | "SUBT"
    | "MULT"
    | "DIV") as op                                          {BINARYOP(op)}

    |("COUNT"
    | "ROWCOUNT"
    | "COLCOUNT"
    | "SUM"
    | "ROWSUM"
    | "COLSUM"
    | "AVG"
    | "ROWAVG"
    | "COLAVG"
    | "MIN"
    | "ROWMIN"
    | "COLMIN"
    | "MAX"
    | "ROWMAX"
    | "COLMAX") as op                                       {UNARYOP(op)}

    | eof                                                   {raise EOF}



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
                | INT(x) -> Printf.bprintf outbuf "INT \n%d \n" x
                | FLOAT(x) -> Printf.bprintf outbuf "FLOAT \n%f \n" x
                | BINARYOP(x) -> Printf.bprintf outbuf "BINARYOP \n%s \n" x
                | UNARYOP(x) -> Printf.bprintf outbuf "UNARYOP \n%s \n" x
                | ASSIGNMENT -> Printf.bprintf outbuf "ASSIGNMENT \n"
                | SEMICOLON -> Printf.bprintf outbuf "SEMICOLON \n"
                | LPAREN -> Printf.bprintf outbuf "LPAREN \n"
                | RPAREN -> Printf.bprintf outbuf "RPAREN \n"
                | LBRACKET -> Printf.bprintf outbuf "LBRACKET \n"
                | RBRACKET -> Printf.bprintf outbuf "RBRACKET \n"
                | COLON -> Printf.bprintf outbuf "COLON \n"
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

