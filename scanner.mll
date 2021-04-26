(* Ocamllex scanner for JQER *)

{
  open Jqerparse
  open Lexing

  let unescape s =
      Scanf.sscanf ("\"" ^ s ^ "\"") "%S%!" (fun x -> x)

(* Referenced for using stack: https://github.com/LibAssignment/INF564-assignment2 *)
  let stack = ref [0]  (* indentation stack *)
  let rec unindent n = match !stack with
    | m :: _ when m = n -> []
    | m :: st when m > n -> stack := st; END :: unindent n
    | _ -> raise (Failure "bad indentation")
  let update_stack n =
    match !stack with
    | m :: _ when m < n ->
      stack := n :: !stack;
      [NEWLINE; BEGIN]
    | _ ->
      NEWLINE :: unindent n
 }

let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let ident = letter (letter | digit | '_')*
let digits = digit+
let ascii = ([' '-'!' '#'-'[' ']'-'~'])
let escape = '\\' ['\\' ''' '"' 'n' 'r' 't']
let string = '"' ( (ascii | escape)* as s) '"'
let space = ' ' | '\t'
let comment = "#" [^'\n']*

rule token = parse
| '\n'    { new_line lexbuf; update_stack (indentation lexbuf) }
| '\r' { token lexbuf } (* Whitespace *)
(* | '#'     { comment lexbuf }           Comments *)
| (space | comment)+ { token lexbuf }
| '('      { [LPAREN] }
| ')'      { [RPAREN] }
| '{'      { [LBRACE] }
| '}'      { [RBRACE] }
| '['      { [LSQUARE] }
| ']'      { [RSQUARE] }
| ':'      { [COLON] }
| ','      { [COMMA] }
| '+'      { [PLUS] }
| '-'      { [MINUS] }
| '*'      { [TIMES] }
| '/'      { [DIVIDE] }
| '%'      { [MODULUS] }
| '='      { [ASSIGN] }
| "=="     { [EQ] }
| "!="     { [NEQ] }
| "<"      { [LT] }
| "<="     { [LEQ] }
| ">"      { [GT] }
| ">="     { [GEQ] }
| "and"     { [AND] }
| "or"     { [OR] }
| "!"      { [NOT] }
| ";"      { [SEMI] }
| "."      { [DOT] }
| "def"    { [DEF] }
| "if"     { [IF] }
| "else"   { [ELSE] }
| "for"    { [FOR] }
| "while"  { [WHILE] }
| "return" { [RETURN] }
| "int"    { [INT] }
| "bool"   { [BOOL] }
| "none"   { [NONE] }
| "str"    { [STRING] }
| "true"   { [BLIT(true)]  }
| "false"  { [BLIT(false)] }
| "print"  { [PRINT] }
| "tuple"  { [TUPLE] }
| digits as lxm { [INT_LITERAL(int_of_string lxm)] }
| ident as lxm { [ID(lxm)] }
| string            { [STRING_LITERAL( (unescape s) )] }
| eof { [EOF] }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

(* and comment = parse
  '\n' { token lexbuf }
| _    { comment lexbuf } *)

and indentation = parse
  | (space | comment)* '\n'
      { new_line lexbuf; indentation lexbuf }
  | space* as s
      { String.length s }


{
  let next_token =
    let tokens = Queue.create () in
    fun lb ->
      if Queue.is_empty tokens then begin
	let l = token lb in
	List.iter (fun t -> Queue.add t tokens) l
      end;
      Queue.pop tokens
}