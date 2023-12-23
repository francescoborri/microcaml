{
  open Parser

  exception LexicalError of string
}

rule read = parse
  | [' ' '\t']+ | '\r' | '\n' | "\r\n"
    { read lexbuf }
  | ('-'? ['0'-'9']+) as num
    { INT (int_of_string num) }
  | '\"' ([^ '\"' '\\']* as str) '\"'
    { STRING str }
  | "true" { TRUE }
  | "false" { FALSE }
  | '+' { PLUS }
  | '-' { MINUS }
  | '*' { TIMES }
  | '/' { DIV }
  | "mod" { MOD }
  | '=' { EQUAL }
  | '<' { LESS }
  | '>' { GREATER }
  | "and" { AND }
  | "or" { OR }
  | "not" { NOT }
  | '^' { CONCAT }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "let" { LET }
  | "rec" { REC }
  | "in" { IN }
  | "print" { PRINT }
  | "->" { ARROW }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | ',' { COMMA }
  | eof { EOF }
  | (['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*) as ident
    { IDENT ident }
  | _ as unknown
    { raise (LexicalError ("Unknown symbol " ^ (String.make 1 unknown))) }