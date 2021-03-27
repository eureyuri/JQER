type token =
  | SEMI
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
  | DEF
  | END
  | RETURN
  | IF
  | ELSE
  | FOR
  | WHILE
  | INT
  | BOOL
  | FLOAT
  | LIST
  | NONE
  | STRING
  | PRINT
  | LITERAL of (int)
  | BLIT of (bool)
  | ID of (string)
  | FLIT of (string)
  | STRING_LITERAL of (string)
  | EOF

open Parsing;;
let _ = parse_error;;
# 4 "jqerparse.mly"
open! Ast
# 53 "jqerparse.ml"
let yytransl_const = [|
  257 (* SEMI *);
  258 (* COLON *);
  259 (* LPAREN *);
  260 (* RPAREN *);
  261 (* LBRACE *);
  262 (* RBRACE *);
  263 (* LSQUARE *);
  264 (* RSQUARE *);
  265 (* COMMA *);
  266 (* PLUS *);
  267 (* MINUS *);
  268 (* TIMES *);
  269 (* DIVIDE *);
  270 (* MODULUS *);
  271 (* ASSIGN *);
  272 (* NOT *);
  273 (* EQ *);
  274 (* NEQ *);
  275 (* LT *);
  276 (* LEQ *);
  277 (* GT *);
  278 (* GEQ *);
  279 (* AND *);
  280 (* OR *);
  281 (* EOL *);
  282 (* DEF *);
  283 (* END *);
  284 (* RETURN *);
  285 (* IF *);
  286 (* ELSE *);
  287 (* FOR *);
  288 (* WHILE *);
  289 (* INT *);
  290 (* BOOL *);
  291 (* FLOAT *);
  292 (* LIST *);
  293 (* NONE *);
  294 (* STRING *);
  295 (* PRINT *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  296 (* LITERAL *);
  297 (* BLIT *);
  298 (* ID *);
  299 (* FLIT *);
  300 (* STRING_LITERAL *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\004\000\006\000\006\000\009\000\
\009\000\005\000\005\000\005\000\005\000\005\000\005\000\007\000\
\007\000\003\000\008\000\008\000\010\000\010\000\010\000\010\000\
\010\000\010\000\010\000\010\000\012\000\012\000\011\000\011\000\
\011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
\011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
\011\000\011\000\011\000\011\000\011\000\013\000\013\000\014\000\
\014\000\000\000"

let yylen = "\002\000\
\002\000\000\000\002\000\002\000\010\000\000\000\001\000\002\000\
\004\000\001\000\001\000\001\000\001\000\001\000\004\000\000\000\
\002\000\003\000\000\000\002\000\002\000\003\000\003\000\005\000\
\007\000\009\000\005\000\005\000\000\000\001\000\001\000\001\000\
\001\000\001\000\001\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\002\000\002\000\003\000\004\000\003\000\000\000\001\000\001\000\
\003\000\002\000"

let yydefred = "\000\000\
\002\000\000\000\058\000\000\000\010\000\011\000\012\000\000\000\
\013\000\014\000\001\000\003\000\004\000\000\000\000\000\000\000\
\000\000\000\000\000\000\018\000\015\000\000\000\000\000\000\000\
\000\000\008\000\000\000\000\000\016\000\000\000\000\000\009\000\
\017\000\000\000\000\000\019\000\000\000\000\000\000\000\005\000\
\000\000\000\000\000\000\000\000\000\000\031\000\033\000\000\000\
\032\000\034\000\020\000\000\000\000\000\000\000\049\000\050\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\021\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\023\000\053\000\
\022\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\038\000\039\000\040\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\052\000\000\000\000\000\000\000\027\000\028\000\
\000\000\000\000\000\000\025\000\000\000\000\000\026\000"

let yydgoto = "\002\000\
\003\000\004\000\012\000\013\000\014\000\024\000\031\000\035\000\
\025\000\051\000\052\000\058\000\087\000\088\000"

let yysindex = "\007\000\
\000\000\000\000\000\000\001\000\000\000\000\000\000\000\006\255\
\000\000\000\000\000\000\000\000\000\000\235\254\027\255\230\254\
\021\255\022\255\041\255\000\000\000\000\027\255\014\255\049\255\
\057\255\000\000\056\255\027\255\000\000\035\255\027\255\000\000\
\000\000\036\255\043\255\000\000\007\255\007\255\007\255\000\000\
\007\255\076\255\085\255\092\255\096\255\000\000\000\000\004\255\
\000\000\000\000\000\000\190\255\065\255\136\000\000\000\000\000\
\231\000\090\255\007\255\007\255\007\255\007\255\007\255\007\255\
\000\000\007\255\007\255\007\255\007\255\007\255\007\255\007\255\
\007\255\007\255\007\255\007\255\007\255\007\255\000\000\000\000\
\000\000\157\000\112\255\178\000\199\000\231\000\106\255\108\255\
\231\000\088\255\088\255\000\000\000\000\000\000\017\001\017\001\
\231\255\231\255\231\255\231\255\004\001\246\000\131\255\007\255\
\131\255\120\255\000\000\007\255\093\255\205\255\000\000\000\000\
\231\000\131\255\007\255\000\000\118\255\131\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\128\255\000\000\000\000\
\135\255\000\000\000\000\000\000\000\000\000\000\087\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\123\255\000\000\000\000\000\000\000\000\000\000\000\000\175\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\010\255\000\000\000\000\123\255\000\000\000\000\139\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\011\255\000\000\140\255\
\008\255\229\255\253\255\000\000\000\000\000\000\119\000\121\000\
\023\000\047\000\071\000\095\000\157\255\049\000\000\000\000\000\
\000\000\000\000\000\000\000\000\109\255\000\000\000\000\000\000\
\048\255\000\000\141\255\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\104\000\000\000\209\001\000\000\000\000\110\000\
\000\000\051\000\221\255\196\255\000\000\000\000"

let yytablesize = 551
let yytable = "\083\000\
\011\000\054\000\055\000\056\000\016\000\057\000\063\000\001\000\
\051\000\037\000\030\000\051\000\015\000\030\000\056\000\019\000\
\051\000\038\000\064\000\056\000\017\000\020\000\039\000\082\000\
\057\000\084\000\085\000\086\000\089\000\021\000\090\000\091\000\
\092\000\093\000\094\000\095\000\096\000\097\000\098\000\099\000\
\100\000\101\000\102\000\022\000\036\000\037\000\046\000\047\000\
\048\000\049\000\050\000\057\000\027\000\038\000\117\000\026\000\
\057\000\029\000\039\000\005\000\006\000\007\000\008\000\009\000\
\010\000\028\000\036\000\037\000\110\000\040\000\041\000\042\000\
\113\000\043\000\044\000\038\000\032\000\017\000\059\000\057\000\
\039\000\045\000\046\000\047\000\048\000\049\000\050\000\060\000\
\019\000\019\000\081\000\079\000\041\000\042\000\061\000\043\000\
\044\000\019\000\062\000\068\000\069\000\070\000\019\000\045\000\
\046\000\047\000\048\000\049\000\050\000\107\000\024\000\024\000\
\104\000\019\000\019\000\019\000\108\000\019\000\019\000\024\000\
\112\000\118\000\114\000\029\000\024\000\019\000\019\000\019\000\
\019\000\019\000\019\000\006\000\036\000\037\000\033\000\024\000\
\024\000\024\000\007\000\024\000\024\000\038\000\054\000\055\000\
\029\000\053\000\039\000\024\000\024\000\024\000\024\000\024\000\
\024\000\109\000\000\000\111\000\000\000\047\000\041\000\042\000\
\047\000\043\000\044\000\000\000\116\000\047\000\000\000\000\000\
\119\000\045\000\046\000\047\000\048\000\049\000\050\000\035\000\
\000\000\000\000\035\000\047\000\047\000\000\000\000\000\035\000\
\035\000\035\000\035\000\035\000\035\000\000\000\065\000\035\000\
\035\000\035\000\035\000\035\000\035\000\035\000\035\000\066\000\
\067\000\068\000\069\000\070\000\000\000\115\000\071\000\072\000\
\073\000\074\000\075\000\076\000\077\000\078\000\066\000\067\000\
\068\000\069\000\070\000\000\000\000\000\071\000\072\000\073\000\
\074\000\075\000\076\000\077\000\078\000\036\000\000\000\000\000\
\036\000\000\000\000\000\000\000\000\000\036\000\036\000\036\000\
\066\000\067\000\068\000\069\000\070\000\036\000\036\000\036\000\
\036\000\036\000\036\000\036\000\036\000\037\000\000\000\000\000\
\037\000\000\000\000\000\000\000\000\000\037\000\037\000\037\000\
\000\000\000\000\000\000\000\000\000\000\037\000\037\000\037\000\
\037\000\037\000\037\000\037\000\037\000\000\000\000\000\043\000\
\000\000\000\000\043\000\000\000\000\000\000\000\000\000\043\000\
\000\000\005\000\006\000\007\000\008\000\009\000\010\000\043\000\
\043\000\043\000\043\000\043\000\043\000\043\000\043\000\044\000\
\000\000\048\000\044\000\000\000\048\000\000\000\000\000\044\000\
\000\000\048\000\000\000\000\000\000\000\000\000\000\000\044\000\
\044\000\044\000\044\000\044\000\044\000\044\000\044\000\045\000\
\048\000\000\000\045\000\000\000\000\000\000\000\000\000\045\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\045\000\
\045\000\045\000\045\000\045\000\045\000\045\000\045\000\046\000\
\000\000\000\000\046\000\000\000\000\000\000\000\000\000\046\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\046\000\
\046\000\046\000\046\000\046\000\046\000\046\000\046\000\041\000\
\000\000\042\000\041\000\000\000\042\000\000\000\000\000\041\000\
\000\000\042\000\000\000\000\000\000\000\000\000\000\000\041\000\
\041\000\042\000\042\000\080\000\000\000\041\000\041\000\042\000\
\042\000\066\000\067\000\068\000\069\000\070\000\000\000\000\000\
\071\000\072\000\073\000\074\000\075\000\076\000\077\000\078\000\
\103\000\000\000\000\000\000\000\000\000\000\000\066\000\067\000\
\068\000\069\000\070\000\000\000\000\000\071\000\072\000\073\000\
\074\000\075\000\076\000\077\000\078\000\105\000\000\000\000\000\
\000\000\000\000\000\000\066\000\067\000\068\000\069\000\070\000\
\000\000\000\000\071\000\072\000\073\000\074\000\075\000\076\000\
\077\000\078\000\106\000\000\000\000\000\000\000\000\000\000\000\
\066\000\067\000\068\000\069\000\070\000\000\000\000\000\071\000\
\072\000\073\000\074\000\075\000\076\000\077\000\078\000\018\000\
\000\000\000\000\000\000\000\000\000\000\000\000\023\000\000\000\
\000\000\000\000\000\000\000\000\030\000\000\000\000\000\034\000\
\066\000\067\000\068\000\069\000\070\000\000\000\000\000\071\000\
\072\000\073\000\074\000\075\000\076\000\077\000\078\000\066\000\
\067\000\068\000\069\000\070\000\000\000\000\000\071\000\072\000\
\073\000\074\000\075\000\076\000\077\000\066\000\067\000\068\000\
\069\000\070\000\000\000\000\000\071\000\072\000\073\000\074\000\
\075\000\076\000\066\000\067\000\068\000\069\000\070\000\000\000\
\000\000\000\000\000\000\073\000\074\000\075\000\076\000"

let yycheck = "\060\000\
\000\000\037\000\038\000\039\000\026\001\041\000\003\001\001\000\
\001\001\003\001\001\001\004\001\007\001\004\001\004\001\042\001\
\009\001\011\001\015\001\009\001\042\001\001\001\016\001\059\000\
\060\000\061\000\062\000\063\000\064\000\008\001\066\000\067\000\
\068\000\069\000\070\000\071\000\072\000\073\000\074\000\075\000\
\076\000\077\000\078\000\003\001\002\001\003\001\040\001\041\001\
\042\001\043\001\044\001\004\001\004\001\011\001\115\000\042\001\
\009\001\002\001\016\001\033\001\034\001\035\001\036\001\037\001\
\038\001\009\001\002\001\003\001\104\000\027\001\028\001\029\001\
\108\000\031\001\032\001\011\001\042\001\042\001\003\001\115\000\
\016\001\039\001\040\001\041\001\042\001\043\001\044\001\003\001\
\002\001\003\001\001\001\027\001\028\001\029\001\003\001\031\001\
\032\001\011\001\003\001\012\001\013\001\014\001\016\001\039\001\
\040\001\041\001\042\001\043\001\044\001\004\001\002\001\003\001\
\001\001\027\001\028\001\029\001\009\001\031\001\032\001\011\001\
\001\001\004\001\030\001\001\001\016\001\039\001\040\001\041\001\
\042\001\043\001\044\001\004\001\002\001\003\001\031\000\027\001\
\028\001\029\001\004\001\031\001\032\001\011\001\004\001\004\001\
\004\001\036\000\016\001\039\001\040\001\041\001\042\001\043\001\
\044\001\103\000\255\255\105\000\255\255\001\001\028\001\029\001\
\004\001\031\001\032\001\255\255\114\000\009\001\255\255\255\255\
\118\000\039\001\040\001\041\001\042\001\043\001\044\001\001\001\
\255\255\255\255\004\001\023\001\024\001\255\255\255\255\009\001\
\010\001\011\001\012\001\013\001\014\001\255\255\001\001\017\001\
\018\001\019\001\020\001\021\001\022\001\023\001\024\001\010\001\
\011\001\012\001\013\001\014\001\255\255\001\001\017\001\018\001\
\019\001\020\001\021\001\022\001\023\001\024\001\010\001\011\001\
\012\001\013\001\014\001\255\255\255\255\017\001\018\001\019\001\
\020\001\021\001\022\001\023\001\024\001\001\001\255\255\255\255\
\004\001\255\255\255\255\255\255\255\255\009\001\010\001\011\001\
\010\001\011\001\012\001\013\001\014\001\017\001\018\001\019\001\
\020\001\021\001\022\001\023\001\024\001\001\001\255\255\255\255\
\004\001\255\255\255\255\255\255\255\255\009\001\010\001\011\001\
\255\255\255\255\255\255\255\255\255\255\017\001\018\001\019\001\
\020\001\021\001\022\001\023\001\024\001\255\255\255\255\001\001\
\255\255\255\255\004\001\255\255\255\255\255\255\255\255\009\001\
\255\255\033\001\034\001\035\001\036\001\037\001\038\001\017\001\
\018\001\019\001\020\001\021\001\022\001\023\001\024\001\001\001\
\255\255\001\001\004\001\255\255\004\001\255\255\255\255\009\001\
\255\255\009\001\255\255\255\255\255\255\255\255\255\255\017\001\
\018\001\019\001\020\001\021\001\022\001\023\001\024\001\001\001\
\024\001\255\255\004\001\255\255\255\255\255\255\255\255\009\001\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\017\001\
\018\001\019\001\020\001\021\001\022\001\023\001\024\001\001\001\
\255\255\255\255\004\001\255\255\255\255\255\255\255\255\009\001\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\017\001\
\018\001\019\001\020\001\021\001\022\001\023\001\024\001\001\001\
\255\255\001\001\004\001\255\255\004\001\255\255\255\255\009\001\
\255\255\009\001\255\255\255\255\255\255\255\255\255\255\017\001\
\018\001\017\001\018\001\004\001\255\255\023\001\024\001\023\001\
\024\001\010\001\011\001\012\001\013\001\014\001\255\255\255\255\
\017\001\018\001\019\001\020\001\021\001\022\001\023\001\024\001\
\004\001\255\255\255\255\255\255\255\255\255\255\010\001\011\001\
\012\001\013\001\014\001\255\255\255\255\017\001\018\001\019\001\
\020\001\021\001\022\001\023\001\024\001\004\001\255\255\255\255\
\255\255\255\255\255\255\010\001\011\001\012\001\013\001\014\001\
\255\255\255\255\017\001\018\001\019\001\020\001\021\001\022\001\
\023\001\024\001\004\001\255\255\255\255\255\255\255\255\255\255\
\010\001\011\001\012\001\013\001\014\001\255\255\255\255\017\001\
\018\001\019\001\020\001\021\001\022\001\023\001\024\001\015\000\
\255\255\255\255\255\255\255\255\255\255\255\255\022\000\255\255\
\255\255\255\255\255\255\255\255\028\000\255\255\255\255\031\000\
\010\001\011\001\012\001\013\001\014\001\255\255\255\255\017\001\
\018\001\019\001\020\001\021\001\022\001\023\001\024\001\010\001\
\011\001\012\001\013\001\014\001\255\255\255\255\017\001\018\001\
\019\001\020\001\021\001\022\001\023\001\010\001\011\001\012\001\
\013\001\014\001\255\255\255\255\017\001\018\001\019\001\020\001\
\021\001\022\001\010\001\011\001\012\001\013\001\014\001\255\255\
\255\255\255\255\255\255\019\001\020\001\021\001\022\001"

let yynames_const = "\
  SEMI\000\
  COLON\000\
  LPAREN\000\
  RPAREN\000\
  LBRACE\000\
  RBRACE\000\
  LSQUARE\000\
  RSQUARE\000\
  COMMA\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIVIDE\000\
  MODULUS\000\
  ASSIGN\000\
  NOT\000\
  EQ\000\
  NEQ\000\
  LT\000\
  LEQ\000\
  GT\000\
  GEQ\000\
  AND\000\
  OR\000\
  EOL\000\
  DEF\000\
  END\000\
  RETURN\000\
  IF\000\
  ELSE\000\
  FOR\000\
  WHILE\000\
  INT\000\
  BOOL\000\
  FLOAT\000\
  LIST\000\
  NONE\000\
  STRING\000\
  PRINT\000\
  EOF\000\
  "

let yynames_block = "\
  LITERAL\000\
  BLIT\000\
  ID\000\
  FLIT\000\
  STRING_LITERAL\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    Obj.repr(
# 32 "jqerparse.mly"
            ( _1 )
# 385 "jqerparse.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    Obj.repr(
# 35 "jqerparse.mly"
                 ( ([], [])               )
# 391 "jqerparse.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 36 "jqerparse.mly"
               ( ((_2 :: fst _1), snd _1) )
# 399 "jqerparse.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'fdecl) in
    Obj.repr(
# 37 "jqerparse.mly"
               ( (fst _1, (_2 :: snd _1)) )
# 407 "jqerparse.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 9 : 'typ) in
    let _3 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 5 : 'formals_opt) in
    let _8 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_list) in
    let _9 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 41 "jqerparse.mly"
     ( { typ = _1;
	 fname = _3;
	 formals = List.rev _5;
	 locals = List.rev _8;
	 body = List.rev _9 } )
# 422 "jqerparse.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 48 "jqerparse.mly"
                  ( [] )
# 428 "jqerparse.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formal_list) in
    Obj.repr(
# 49 "jqerparse.mly"
                  ( _1 )
# 435 "jqerparse.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 52 "jqerparse.mly"
                             ( [(_1,_2)]     )
# 443 "jqerparse.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'formal_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 53 "jqerparse.mly"
                             ( (_3,_4) :: _1 )
# 452 "jqerparse.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 56 "jqerparse.mly"
          ( Int   )
# 458 "jqerparse.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 57 "jqerparse.mly"
          ( Bool  )
# 464 "jqerparse.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 58 "jqerparse.mly"
          ( Float )
# 470 "jqerparse.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 59 "jqerparse.mly"
          ( None  )
# 476 "jqerparse.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 60 "jqerparse.mly"
           ( String )
# 482 "jqerparse.ml"
               : 'typ))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    Obj.repr(
# 61 "jqerparse.mly"
                             ( List(_3) )
# 489 "jqerparse.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 64 "jqerparse.mly"
                     ( [] )
# 495 "jqerparse.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'vdecl_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 65 "jqerparse.mly"
                     ( _2 :: _1 )
# 503 "jqerparse.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 68 "jqerparse.mly"
               ( (_1, _2) )
# 511 "jqerparse.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 71 "jqerparse.mly"
                   ( [] )
# 517 "jqerparse.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 72 "jqerparse.mly"
                   ( _2 :: _1 )
# 525 "jqerparse.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 75 "jqerparse.mly"
                                            ( Expr _1               )
# 532 "jqerparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr_opt) in
    Obj.repr(
# 76 "jqerparse.mly"
                                            ( Return _2             )
# 539 "jqerparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 77 "jqerparse.mly"
                                            ( Block(List.rev _2)    )
# 546 "jqerparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 78 "jqerparse.mly"
                                            ( If(_3, _5, Block([])) )
# 554 "jqerparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 79 "jqerparse.mly"
                                            ( If(_3, _5, _7)        )
# 563 "jqerparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 6 : 'expr_opt) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'expr_opt) in
    let _9 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 81 "jqerparse.mly"
                                            ( For(_3, _5, _7, _9)   )
# 573 "jqerparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 82 "jqerparse.mly"
                                            ( While(_3, _5)         )
# 581 "jqerparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    Obj.repr(
# 83 "jqerparse.mly"
                                  ( Print(_3) )
# 588 "jqerparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 86 "jqerparse.mly"
                  ( Noexpr )
# 594 "jqerparse.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 87 "jqerparse.mly"
                  ( _1 )
# 601 "jqerparse.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 90 "jqerparse.mly"
                     ( Literal(_1)            )
# 608 "jqerparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 91 "jqerparse.mly"
                    ( Fliteral(_1)           )
# 615 "jqerparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 92 "jqerparse.mly"
                     ( BoolLit(_1)            )
# 622 "jqerparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 93 "jqerparse.mly"
                     ( StringLit(_1) )
# 629 "jqerparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 94 "jqerparse.mly"
                     ( Id(_1)                 )
# 636 "jqerparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 95 "jqerparse.mly"
                     ( Binop(_1, Add,   _3)   )
# 644 "jqerparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 96 "jqerparse.mly"
                     ( Binop(_1, Sub,   _3)   )
# 652 "jqerparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 97 "jqerparse.mly"
                     ( Binop(_1, Mult,  _3)   )
# 660 "jqerparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 98 "jqerparse.mly"
                     ( Binop(_1, Div,   _3)   )
# 668 "jqerparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 99 "jqerparse.mly"
                      ( Binop(_1, Mod,   _3)   )
# 676 "jqerparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 100 "jqerparse.mly"
                     ( Binop(_1, Equal, _3)   )
# 684 "jqerparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 101 "jqerparse.mly"
                     ( Binop(_1, Neq,   _3)   )
# 692 "jqerparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 102 "jqerparse.mly"
                     ( Binop(_1, Less,  _3)   )
# 700 "jqerparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 103 "jqerparse.mly"
                     ( Binop(_1, Leq,   _3)   )
# 708 "jqerparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 104 "jqerparse.mly"
                     ( Binop(_1, Greater, _3) )
# 716 "jqerparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 105 "jqerparse.mly"
                     ( Binop(_1, Geq,   _3)   )
# 724 "jqerparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 106 "jqerparse.mly"
                     ( Binop(_1, And,   _3)   )
# 732 "jqerparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 107 "jqerparse.mly"
                     ( Binop(_1, Or,    _3)   )
# 740 "jqerparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 108 "jqerparse.mly"
                         ( Unop(Neg, _2)      )
# 747 "jqerparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 109 "jqerparse.mly"
                     ( Unop(Not, _2)          )
# 754 "jqerparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 110 "jqerparse.mly"
                     ( Assign(_1, _3)         )
# 762 "jqerparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'args_opt) in
    Obj.repr(
# 111 "jqerparse.mly"
                              ( Call(_1, _3)  )
# 770 "jqerparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 112 "jqerparse.mly"
                       ( _2                   )
# 777 "jqerparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 115 "jqerparse.mly"
                  ( [] )
# 783 "jqerparse.ml"
               : 'args_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'args_list) in
    Obj.repr(
# 116 "jqerparse.mly"
               ( List.rev _1 )
# 790 "jqerparse.ml"
               : 'args_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 119 "jqerparse.mly"
                            ( [_1] )
# 797 "jqerparse.ml"
               : 'args_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'args_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 120 "jqerparse.mly"
                         ( _3 :: _1 )
# 805 "jqerparse.ml"
               : 'args_list))
(* Entry program *)
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
let program (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.program)
