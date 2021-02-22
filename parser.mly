%{
  open Ast
%}

%token LPAREN RPAREN LBRACKET RBRACKET
%token COMMA PLUS MINUS TIMES DIVIDE ASSIGN
%token NOT EQ NEQ LT LEQ GT GEQ AND OR NEG
%token DEF RETURN IF ELSE ELIF NOELSE FOR WHILE
%token CONTINUE BREAK IN
%token INT BOOL CHAR STRING TREE
%token TRUE FALSE NONE
%token TAB COLON INDENT DEDENT

%token <int> INT_LITERAL
%token <bool> BOOL_LITERAL
%token <string> STRING_LITERAL
%token <char> CHAR_LITERAL
%token <string> VARIABLE
%token EOF

%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGNMENT
%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE
%right NOT NEG

%nonassoc LPAREN LBRACKET
%nonassoc RPAREN RBRACKET

%start program
%type <Ast.program> program

%%

program:
  decls EOF { $1 }


expr:
  expr PLUS           expr { Binop($1, Add, $3) }
| expr MINUS          expr { Binop($1, Sub, $3) }
| expr TIMES          expr { Binop($1, Mul, $3) }
| expr DIVIDE         expr { Binop($1, Div, $3) }
| expr SEQUENCE       expr { Seq($1, $3) }
| VARIABLE ASSIGNMENT expr { Assign($1, $3) }
| VARIABLE                 { Var($1) }
| LITERAL                  { Lit($1) }
