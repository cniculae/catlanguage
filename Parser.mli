type token =
  | EOL
  | EOF
  | BEGIN
  | END
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
  | CONCAT
  | TRUE
  | FALSE
  | STRDEL
  | PRINTNL
  | PRINT
  | SET
  | EMPTYSET
  | CREATESET
  | ADDTOSET
  | UNION
  | INTER
  | SETDIF
  | DELETEFROMSET
  | PRINTSET
  | INT of (int)
  | STRING of (string)

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> ParseTree.parsetree
