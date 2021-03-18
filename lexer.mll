{
  open Lexing
  open Ast
  open Parser

  (*exception to indicate the error is coming from Lexer*)
  exception Lexing_error of string

  (*the string buffer*)
  let string_buffer = Buffer.create 1024

  (* indentaion stack *)
  let stack = ref [0]
  let rec unindent n = match !stack with
    | m :: _ when m = n -> []
    | m :: st when m > n -> stack := st; END :: unindent n
    | _ -> raise (Lexing_error "bad indentation")
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
let integer = digit+
let space = ' ' | '\t'
let comment = "#" [^'\n']*

rule next_tokens = parse
  | '\n'    { new_line lexbuf; update_stack (indentation lexbuf) }
  | (space | comment)+
            { next_tokens lexbuf }
  | '+'     { [PLUS] }
  | '-'     { [MINUS] }
  | '*'     { [TIMES] }
  | "//"    { [DIV] }
  (* | '%'     { [MOD] } *)
  | '='     { [EQUAL] }
  | "=="    { [CMP Beq] }
  | "!="    { [CMP Bneq] }
  | "<"     { [CMP Blt] }
  | "<="    { [CMP Ble] }
  | ">"     { [CMP Bgt] }
  | ">="    { [CMP Bge] }
  | '('     { [LP] }
  | ')'     { [RP] }
  | '['     { [LSQ] }
  | ']'     { [RSQ] }
  | ','     { [COMMA] }
  | ':'     { [COLON] }
  | "def"   { [DEF] }
  | "if"    { [IF] }
  | "else"  { [ELSE] }
  | "return"{ [RETURN] }
  | "print" { [PRINT] }
  | "for"   { [FOR] }
  | "in"    { [IN] }
  | "and"   { [AND] }
  | "or"    { [OR] }
  | "not"   { [NOT] }
  | "True"  { [CST (Cbool true)] }
  | "False" { [CST (Cbool false)] }
  | "None"  { [CST Cnone] }
  | "int"    { [INT] }
  | "bool"   { [BOOL] }
  | "string"  { [STRING] }
  | ident as id { [IDENT(id)] }
  | integer as s
            { try [CST (Cint (int_of_string s))]
              with _ -> raise (Lexing_error ("constant too large: " ^ s)) }
  | '"'     { [CST (Cstring (string lexbuf))] }
  | eof     { [EOF] }
  | _ as c  { raise (Lexing_error ("illegal character: " ^ String.make 1 c)) }

and indentation = parse
  | (space | comment)* '\n'
      { new_line lexbuf; indentation lexbuf }
  | space* as s
      { String.length s }

and string = parse
  | '"'
      { let s = Buffer.contents string_buffer in
	Buffer.reset string_buffer;
	s }
  | "\\n"
      { Buffer.add_char string_buffer '\n';
	string lexbuf }
  | "\\\""
      { Buffer.add_char string_buffer '"';
	string lexbuf }
  | _ as c
      { Buffer.add_char string_buffer c;
	string lexbuf }
  | eof
      { raise (Lexing_error "unterminated string") }

{
  (* next lexemes to send back *)
  let next_token =
    let tokens = Queue.create () in
    fun lb ->
      if Queue.is_empty tokens then begin
	let l = next_tokens lb in
	List.iter (fun t -> Queue.add t tokens) l
      end;
      Queue.pop tokens
}
