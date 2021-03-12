%{
  open Ast
%}

//TODO: more built in? (depth...)

%token LPAREN RPAREN LSQAURE RSQUARE
%token COMMA PLUS MINUS TIMES DIVIDE ASSIGN
%token NOT EQ NEQ LT LEQ GT GEQ AND OR NEG
%token DEF RETURN IF ELSE ELIF NOELSE FOR WHILE DOT RANGE IN
%token INT BOOL CHAR STRING TREE NONE
%token TRUE FALSE
%token BEGIN END COLON NEWLINE

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

%nonassoc LPAREN LSQUARE
%nonassoc RPAREN RSQUARE

%start program
%type <Ast.program> program

%%

program:
  stmt_list EOF { List.rev $1 }

stmt_list:
    /* nothing */ { [] }
  | stmt_list stmt { $2 :: $1 }

stmt:
    expr NEWLINE { Expr $1 }
  | typ DEF VARIABLE LPAREN formals_opt RPAREN COLON NEWLINE stmt_block { Func($1, $3, $5, $9) }
  | RETURN expr_opt NEWLINE { Return $2 }
  | IF expr COLON NEWLINE stmt_block %prec NOELSE { If($2, $5, Block([])) }
  | IF expr COLON NEWLINE stmt_block elseif ELSE COLON NEWLINE stmt_block { If($2, $5, $6, $10) }
  | FOR VARIABLE IN RANGE LPAREN expr RPAREN COLON NEWLINE stmt_block { For($2, $6, $10) }
  | WHILE expr COLON NEWLINE stmt_block { While($2, $5) }

elseif:
    { [] }
  | elseif_list { $1 }

elseif_list:
    ELIF expr COLON NEWLINE stmt_block { [($2, $5)] }
  | elseif_list ELIF expr COLON NEWLINE stmt_block { ($3, $6) :: $1 }


typ:
    INT     { Int     }
  | BOOL    { Bool    }
  | CHAR    { Char    }
  | STRING  { String  }
  | NONE    { None    }
  | TREE LSQUARE typ typ typ RSQUARE    { Tree }

stmt_block:
  | BEGIN stmt_list END { Statement_List(List.rev $2) }
  // TODO: currently we can have a def inside a def

formals_opt:
    /* nothing */ { [] }
  | formal_list   { $1 }

formal_list:
    typ VARIABLE                   { [($1,$2)]     }
  | formal_list COMMA typ VARIABLE { ($3,$4) :: $1 }

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
| NOT                 expr            { Unop(Not, $2)          }
| MINUS expr %prec NEG                { Unop(Neg, $2)          }
| INT_LITERAL                         { IntLit($1)             }
| BOOL_LITERAL                        { BoolLit($1)            }
| STRING_LITERAL                      { StringLit($1)          }
| CHAR_LITERAL                        { CharLit($1)            }
| NONE                                { NONE                   }
| tree_struct                         { Tree($1)               }
| LPAREN expr RPAREN                  { $2                     }
| VARIABLE LPAREN args_opt RPAREN     { Call($1, $3)           } // function call

args_opt:
    /* nothing */ { [] }
  | args_list  { List.rev $1 }

args_list:
    expr                    { [$1] }
  | args_list COMMA expr    { $3 :: $1 }

//TODO: refine the tree struct to not break the definition
tree_struct:
    NONE      { None }
  |  LSQUARE tree_value tree_struct tree_struct RSQUARE { ($2, $3, $4) }

tree_value:
    INT_LITERAL                     { IntLit($1)             }
  | BOOL_LITERAL                    { BoolLit($1)            }
  | STRING_LITERAL                  { StringLit($1)          }
  | CHAR_LITERAL                    { CharLit($1)            }