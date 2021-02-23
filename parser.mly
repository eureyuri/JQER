%{
  open Ast
%}

%token LPAREN RPAREN LBRACKET RBRACKET SEP
%token COMMA PLUS MINUS TIMES DIVIDE ASSIGN
%token NOT EQ NEQ LT LEQ GT GEQ AND OR NEG
%token DEF RETURN IF ELSE ELIF NOELSE FOR WHILE DOT RANGE
%token CONTINUE BREAK IN
%token INT BOOL CHAR STRING TREE VOID
%token TRUE FALSE NONE
%token TAB COLON INDENT DEDENT

%token <int> INT_LITERAL
%token <bool> BOOL_LITERAL
%token <string> STRING_LITERAL
%token <char> CHAR_LITERAL
%token <string> VARIABLE
%token EOF

// Precedence
%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE
%right NOT NEG
%left DOT

%nonassoc LPAREN LBRACKET
%nonassoc RPAREN RBRACKET

%start program
%type <Ast.program> program

%%

program:
  stmt_list EOF { List.rev $1 }

stmt_list:
    /* nothing */ { [] }
  | stmt_list stmt { $2 :: $1 }

// TODO: How do we do elif
stmt:
    expr SEP { Expr $1 }
  | DEF VARIABLE LPAREN formals_opt RPAREN COLON stmt_block { Func($2, $4, $7) }
  | RETURN expr_opt SEP { Return $2 }
  | IF expr COLON stmt_block %prec NOELSE { If($2, $4, Block([])) }
  | IF expr COLON stmt_block ELSE COLON stmt_block { If($2, $4, $7) }
  | FOR VARIABLE IN RANGE LPAREN expr RPAREN COLON stmt_block { For($2, $6, $9) }
  | WHILE expr COLON stmt_block { While($2, $4) }

stmt_block:
  | INDENT SEP stmt_list DEDENT { Statement_List(List.rev $3) }

formals_opt:
    /* nothing */ { [] }
  | formal_list   { $1 }

// TODO: Fix this
formal_list:
    typ VARIABLE                   { [($1,$2)]     }
  | formal_list COMMA typ VARIABLE { ($3,$4) :: $1 }

typ:
    INT     { Int     }
  | BOOL    { Bool    }
  | CHAR    { Char    }
  | VOID    { Void    }
  | STRING  { String  }
  | TREE    { Tree    }

expr_opt:
    /* nothing */ { Noexpr }
  | expr          { $1 }

expr:
  expr PLUS           expr            { Binop($1, Add, $3)     }
| expr MINUS          expr            { Binop($1, Sub, $3)     }
| expr TIMES          expr            { Binop($1, Mul, $3)     }
| expr DIVIDE         expr            { Binop($1, Div, $3)     }
| expr EQ             expr            { Binop($1, Equal, $3)   }
| expr NEQ            expr            { Binop($1, Neq,   $3)   }
| expr LT             expr            { Binop($1, Less,  $3)   }
| expr LEQ            expr            { Binop($1, Leq,   $3)   }
| expr GT             expr            { Binop($1, Greater, $3) }
| expr GEQ            expr            { Binop($1, Geq,   $3)   }
| expr AND            expr            { Binop($1, And,   $3)   }
| expr OR             expr            { Binop($1, Or,    $3)   }
| VARIABLE ASSIGN     expr            { Assign($1, $3)         }
| VARIABLE                            { Var($1)                }
| NOT expr                            { Unop(Not, $2)          }
| MINUS expr %prec NEG                { Unop(Neg, $2)          }
| INT_LITERAL                         { IntLit($1)             }
| BOOL_LITERAL                        { BoolLit($1)            }
| STRING_LITERAL                      { StringLit($1)          }
| CHAR_LITERAL                        { CharLit($1)            }
| LBRACKET args_opt RBRACKET          { TreeLit($2)            }
| LPAREN expr RPAREN                  { $2                     }
| VARIABLE LPAREN args_opt RPAREN     { Call($1, $3)           } // function call
| VARIABLE DOT LPAREN args_opt RPAREN { Method($1, $4)         } // method call on var

args_opt:
    /* nothing */ { [] }
  | args_list  { List.rev $1 }

args_list:
    expr                    { [$1] }
  | args_list COMMA expr    { $3 :: $1 }