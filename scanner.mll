{
  open Lexing
  open Ast
  open Parser

  exception Lexing_error of string

  (* convert keywordString to keyword token*)
  let id_or_kwd =
    let h = Hashtbl.create 32 in
    List.iter (fun (s, tok) -> Hashtbl.add h s tok)
      ["def", DEF; "if", IF; "else", ELSE;
       "return", RETURN; "print", PRINT;
       "for", FOR; "in", IN;
       "and", AND; "or", OR; "not", NOT;
       "True", CST (Cbool true);
       "False", CST (Cbool false);
       "None", CST Cnone;];
   fun s -> try Hashtbl.find h s with Not_found -> IDENT s

  (* string size *)
  let string_buffer = Buffer.create 1024

  (* indentation stack *)
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
let integer = ['0'-'9']+
let space = ' ' | '\t' (*TODO: Do space or not? *)
let comment = "#" [^'\n']*
let newline = '\n' | '\r'

rule next_tokens = parse
  | newline { new_line lexbuf; update_stack (indentation lexbuf) }
  | (space | comment)+
            { next_tokens lexbuf }
  | ident as id { id_or_kwd id }
  | '+' { PLUS }
  | '-' { MINUS }
  | '*' { TIMES }
  | '/' { DIVIDE }
  | '=' { ASSIGNMENT }
  | "=="{ EQ }
  | "!="{ NEQ }
  | "<" { LT }
  | "<="{ LEQ }
  | ">" { GT }
  | ">="{ GEQ }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '[' { LSQAURE }
  | ']' { RSQUARE }
  | ',' { COMMA }
  | ':' { COLON }
  | '"' { string lexbuf } (*TODO: They defined Cstring to indicate constant, do we need?*)
  | digit + as lit { LITERAL(int_of_string lit) }
  | letter + as lit { VARIABLE(lit) }
  | eof { EOF }
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

  let next_token =
    let tokens = Queue.create () in (* next lexemes to send back *)
    fun lb ->
      if Queue.is_empty tokens then begin
	let l = next_tokens lb in
	List.iter (fun t -> Queue.add t tokens) l
      end;
      Queue.pop tokens
}
