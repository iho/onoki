{
  open Parser

  exception LexError of string

  let keyword_table = Hashtbl.create 32
  let () =
    List.iter (fun (kwd, tok) -> Hashtbl.add keyword_table kwd tok)
      [ "let", LET
      ; "rec", REC
      ; "in", IN
      ; "fun", FUN
      ; "if", IF
      ; "then", THEN
      ; "else", ELSE
      ; "match", MATCH
      ; "with", WITH
      ; "type", TYPE
      ; "module", MODULE
      ; "open", OPEN
      ; "include", INCLUDE
      ; "external", EXTERNAL
      ; "true", TRUE
      ; "false", FALSE
      ; "and", AND
      ; "or", OR
      ; "not", NOT
      ; "of", OF
      ]
}

let digit = ['0'-'9']
let lower_alpha = ['a'-'z' '_']
let upper_alpha = ['A'-'Z']
let alnum = lower_alpha | upper_alpha | digit | '\''
let whitespace = [' ' '\t' '\r']
let newline = '\n' | "\r\n"

rule token = parse
  | whitespace+     { token lexbuf }
  | newline         { Lexing.new_line lexbuf; token lexbuf }
  | "(*"            { comment lexbuf; token lexbuf }
  | digit+ as i     { INT (int_of_string i) }
  | digit+ '.' digit+ as f { FLOAT (float_of_string f) }
  | '"'             { read_string (Buffer.create 16) lexbuf }
  | '('             { LPAREN }
  | ')'             { RPAREN }
  | '['             { LBRACKET }
  | ']'             { RBRACKET }
  | '{'             { LBRACE }
  | '}'             { RBRACE }
  | '+'             { PLUS }
  | '-'             { MINUS }
  | '*'             { STAR }
  | '/'             { SLASH }
  | '%'             { MOD }
  | '^'             { HAT }
  | '='             { EQ }
  | "<>"            { NE }
  | '<'             { LT }
  | "<="            { LE }
  | '>'             { GT }
  | ">="            { GE }
  | "->"            { ARROW }
  | "::"            { CONS }
  | ':'             { COLON }
  | '.'             { DOT }
  | ';'             { SEMICOLON }
  | ','             { COMMA }
  | '|'             { PIPE }
  | '_'             { UNDERSCORE }
  
  | lower_alpha alnum* as id 
      { try Hashtbl.find keyword_table id 
        with Not_found -> LIDENT id }
  
  | upper_alpha alnum* as id
      { UIDENT id }

  | eof             { EOF }
  | _ as c          { raise (LexError (Printf.sprintf "Unexpected character: %c" c)) }

and comment = parse
  | "*)"            { () }
  | "(*"            { comment lexbuf; comment lexbuf }
  | newline         { Lexing.new_line lexbuf; comment lexbuf }
  | _               { comment lexbuf }
  | eof             { raise (LexError "Unterminated comment") }

and read_string buf = parse
  | '"'             { STRING (Buffer.contents buf) }
  | '\\' '\\'       { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' '"'        { Buffer.add_char buf '"'; read_string buf lexbuf }
  | '\\' 'n'        { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 't'        { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | newline         { Lexing.new_line lexbuf; Buffer.add_char buf '\n'; read_string buf lexbuf }
  | _ as c          { Buffer.add_char buf c; read_string buf lexbuf }
  | eof             { raise (LexError "Unterminated string") }
