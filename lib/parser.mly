%{
  open Interpreter
%}

%token <string> IDENT
%token <int> INT
%token TRUE FALSE
%token <string> STRING
%token PLUS MINUS TIMES DIV MOD
%token EQUAL LESS GREATER
%token AND OR NOT
%token CONCAT
%token IF THEN ELSE LET REC IN PRINT
%token ARROW LPAREN RPAREN COMMA EOF

%nonassoc IN
%nonassoc ELSE
%nonassoc COMMA
%nonassoc PRINT
%left CONCAT
%left AND OR
%left EQUAL LESS GREATER
%left PLUS MINUS
%left TIMES DIV
%left MOD
%left CALL

%type <Interpreter.expr> expr
%type <Interpreter.expr> start

%start start

%%

let start := ~ = expr ; EOF ; <>

let expr :=
  | int = INT ; < Int >
  | TRUE ; { True }
  | FALSE ; { False }
  | str = STRING ; < String >
  | expr1 = expr ; PLUS ; expr2 = expr ; < Add >
  | expr1 = expr ; MINUS ; expr2 = expr ; < Sub >
  | expr1 = expr ; TIMES ; expr2 = expr ; < Mul >
  | expr1 = expr ; DIV ; expr2 = expr ; < Div >
  | expr1 = expr ; MOD ; expr2 = expr ; < Mod >
  | expr1 = expr ; EQUAL ; expr2 = expr ; < Equal >
  | expr1 = expr ; LESS ; expr2 = expr ; < LessThan >
  | expr1 = expr ; GREATER ; expr2 = expr ; < GreaterThan >
  | expr1 = expr ; AND ; expr2 = expr ; < And >
  | expr1 = expr ; OR ; expr2 = expr ; < Or >
  | NOT ; ~ = expr ; < Not >
  | expr1 = expr ; CONCAT ; expr2 = expr ; < Concat >
  | arg = IDENT ; ARROW ; ~ = expr ; < Fun >
  | ident = IDENT ; < Den >
  | IF ; expr1 = expr ; THEN ; expr2 = expr ; ELSE ; expr3 = expr ; < IfThenElse >
  | LET ; ident = IDENT ; EQUAL ; expr1 = expr ; IN ; expr2 = expr ; < Let >
  | LET ; REC ; func = IDENT ; EQUAL ; arg = IDENT ; ARROW ; expr1 = expr ; IN ; expr2 = expr ; < LetRec >
  | expr1 = expr ; expr2 = expr ; < Call > %prec CALL
  | expr1 = expr ; COMMA ; expr2 = expr ; < Sequence >
  | PRINT ; ~ = expr ; < Print >
  | LPAREN; RPAREN ; { Unit }
  | LPAREN ; ~ = expr ; RPAREN ; <>

%%