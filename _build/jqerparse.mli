type token =
  | COLON
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | LSQUARE
  | RSQUARE
  | COMMA
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | MODULUS
  | ASSIGN
  | NOT
  | EQ
  | NEQ
  | LT
  | LEQ
  | GT
  | GEQ
  | AND
  | OR
  | EOL
  | DOT
  | DEF
  | BEGIN
  | END
  | NEWLINE
  | RETURN
  | IF
  | ELSE
  | FOR
  | WHILE
  | INT
  | BOOL
  | TUPLE
  | NONE
  | STRING
  | PRINT
  | SEMI
  | INT_LITERAL of (int)
  | BLIT of (bool)
  | ID of (string)
  | STRING_LITERAL of (string)
  | TUPLE_LITERAL of (int * int)
  | EOF

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
