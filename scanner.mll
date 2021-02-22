(* For matching letters for VARIABLE
    https://stackoverflow.com/questions/3617797/regex-to-match-only-letters *)

{ open Parser }

rule tokenize = parse
  [' ' '\t' '\r' '\n'] { tokenize lexbuf }
| '+' { PLUS }
| '-' { MINUS }
| '*' { TIMES }
| '/' { DIVIDE }
| '=' { ASSIGNMENT }
| ';' { SEQUENCE }
| ['0'-'9']+ as lit { LITERAL(int_of_string lit) }
| ['a'-'z' 'A'-'Z']+ as lit { VARIABLE(lit) }
| eof { EOF }
