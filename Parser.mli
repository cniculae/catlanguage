type token =
  | LCURLY
  | RCURLY
  | LPAREN
  | RPAREN
  | COMMA
  | LEN
  | FIND
  | READINT
  | READSET
  | VAR
  | INT of (int)
  | STRING of (string)
  | ASSIGN
  | MULT
  | DIV
  | PLUS
  | DIF
  | MOD
  | PLUSEQ
  | DIFEQ
  | DIVEQ
  | MULTEQ
  | COMM
  | NOTEQ
  | SMALLER
  | BIGGER
  | SMOREQ
  | BIGOREQ
  | EQUAL
  | NOT
  | AND
  | OR
  | IF
  | WHILE
  | EOL
  | EOF
  | CONCAT
  | TRUE
  | FALSE
  | STRDEL
  | PRINTNL
  | PRINT
  | SET
  | ADDTOSET
  | CREATESET
  | PRINTSET
  | UNION
  | INTER
  | SETDIF
  | EMPTYSET
  | DELETEFROMSET
  | BEGIN
  | END

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> ParseTree.parsetree
