/* Ocamlyacc parser for JQER */

%{
open! Ast
%}

%token COLON LPAREN RPAREN LBRACE RBRACE LSQUARE RSQUARE COMMA PLUS MINUS TIMES DIVIDE MODULUS ASSIGN
%token NOT EQ NEQ LT LEQ GT GEQ AND OR EOL DOT
%token DEF BEGIN END NEWLINE RETURN IF ELSE FOR WHILE INT BOOL TUPLE NONE STRING PRINT SEMI
%token <int> INT_LITERAL
%token <bool> BLIT
%token <string> ID STRING_LITERAL
%token <int * int> TUPLE_LITERAL 
%token EOF

%start program
%type <Ast.program> program

%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE MODULUS
%right NOT

%%

program:
  decls EOF { $1 }

decls:
   /* nothing */ { ([], [])               }
 | decls vdecl { (($2 :: fst $1), snd $1) }
 | decls fdecl { (fst $1, ($2 :: snd $1)) }

fdecl:
   typ DEF ID LPAREN formals_opt RPAREN COLON NEWLINE BEGIN vdecl_list stmt_list END
     { { typ = $1;
	 fname = $3;
	 formals = List.rev $5;
	 locals = List.rev $10;
	 body = List.rev $11 } }

formals_opt:
    /* nothing */ { [] }
  | formal_list   { $1 }

formal_list:
    typ ID                   { [($1,$2)]     }
  | formal_list COMMA typ ID { ($3,$4) :: $1 }

typ:
    INT   { Int   }
  | BOOL  { Bool  }
  | NONE  { None  }
  | STRING { String }
  | TUPLE { Tuple } 

vdecl_list:
    /* nothing */    { [] }
  | vdecl_list vdecl { $2 :: $1 }

vdecl:
   typ ID NEWLINE { ($1, $2) }

stmt_list:
    /* nothing */  { [] }
  | stmt_list stmt { $2 :: $1 }

stmt:
    expr NEWLINE                              { Expr $1               }
  | RETURN expr_opt NEWLINE                   { Return $2             }
  | COLON NEWLINE BEGIN stmt_list END                     { Block(List.rev $4)    }
  | IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7)        }
  | FOR LPAREN expr_opt SEMI expr SEMI expr_opt RPAREN stmt
                                            { For($3, $5, $7, $9)   }
  | WHILE LPAREN expr RPAREN stmt           { While($3, $5)         }
  | PRINT LPAREN expr RPAREN NEWLINE { Print($3) }

expr_opt:
    /* nothing */ { Noexpr }
  | expr          { $1 }

expr:
    INT_LITERAL          { IntLiteral($1)            }
  | BLIT             { BoolLit($1)            }
  | STRING_LITERAL   { StringLit($1) }
  | ID               { Id($1)                 }
  | expr PLUS   expr { Binop($1, Add,   $3)   }
  | expr MINUS  expr { Binop($1, Sub,   $3)   }
  | expr TIMES  expr { Binop($1, Mult,  $3)   }
  | expr DIVIDE expr { Binop($1, Div,   $3)   }
  | expr MODULUS expr { Binop($1, Mod,   $3)   }
  | expr EQ     expr { Binop($1, Equal, $3)   }
  | expr NEQ    expr { Binop($1, Neq,   $3)   }
  | expr LT     expr { Binop($1, Less,  $3)   }
  | expr LEQ    expr { Binop($1, Leq,   $3)   }
  | expr GT     expr { Binop($1, Greater, $3) }
  | expr GEQ    expr { Binop($1, Geq,   $3)   }
  | expr AND    expr { Binop($1, And,   $3)   }
  | expr OR     expr { Binop($1, Or,    $3)   }
  | MINUS expr %prec NOT { Unop(Neg, $2)      }
  | NOT expr         { Unop(Not, $2)          }
  | ID ASSIGN expr   { Assign($1, $3)         }
  | ID LPAREN args_opt RPAREN { Call($1, $3)  }
  | LPAREN expr RPAREN { $2                   }
  | ID DOT INT_LITERAL  { TupleAccess($1, $3)} 
  | LPAREN expr COMMA expr RPAREN { TupleLiteral($2,$4) } 

args_opt:
    /* nothing */ { [] }
  | args_list  { List.rev $1 }

args_list:
    expr                    { [$1] }
  | args_list COMMA expr { $3 :: $1 }
