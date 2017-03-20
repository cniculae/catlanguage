(* File lexer.mll *)
{
open Parser        (* The type token is defined in parser.mli *)
exception Eof
}
rule token = parse
    |"^-.-^"['a'-'z''A'-'Z''0'-'9'' ''_'',']+"^-.-^"   {token lexbuf} (* skip comments *)
    |[' ' '\t']       {token lexbuf} (* skip blanks *)
    |['\n']           {token lexbuf} (* skip blanks *)
    |['\r'] ['\n']   {token lexbuf} (* skip blanks *)
    |"catDo^o.o^" {BEGIN}
    |"catDone^=.=^" {END}
    | ','  { COMMA }
    | ';'  { EOL }
    | 'U'      { UNION }
    | 'n'      { INTER }
    | "setdif"    { SETDIF }

    | "+="     { PLUSEQ }
    | "-="     { DIFEQ }
    | "*="     { MULTEQ }
    | "/"      { DIVEQ }
    | '+'      { PLUS }
    | '-'      { DIF }
    | '*'      { MULT }
    | '/'      { DIV }
    | '%'      { MOD }

    | '.'      { CONCAT }

    | "TRUE" { TRUE } | "true" { TRUE }
    | "FALSE" { FALSE } | "false" { FALSE }

    | "<=" { SMOREQ } | ">=" { BIGOREQ }
    | "!=" { NOTEQ } | "==" { EQUAL }
    | "<"  { SMALLER } | ">"  { BIGGER }
    | '!'       { NOT } | "&&"      { AND } | "||"      { OR }

    | '=' { ASSIGN }

    | "if"     { IF }
    | "len"    { LEN }
    | "find"   { FIND }
    | "while"  { WHILE }
    | "print"  { PRINT }
    | "printnl" { PRINTNL }
    | "readint" { READINT }
    | "readset" { READSET }
    | "printset" { PRINTSET }
    | "createset" { CREATESET }
    | "emptyset" { EMPTYSET }
    | "add" { ADDTOSET }
    | "delete" { DELETEFROMSET }
    | "set" { SET }
    | "var" { VAR }
    | '"'      { STRDEL }
    | '('       { LPAREN }  | ')'       { RPAREN }
    | '{'       { LCURLY }  | '}'       { RCURLY }

    | ['0'-'9']+ as lxm { INT(int_of_string lxm) }
    | ['a'-'z''A'-'Z']+ as str { STRING(str) }

    | eof      { EOF }
    | '#'      { EOF }
