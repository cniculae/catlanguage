(* File lexer.mll *)
{
open Parser        (* The type token is defined in parser.mli *)
exception Eof
}
rule token = parse
    |[' ' '\t']       {token lexbuf} (* skip blanks *)
    |['\n']           {token lexbuf} (* skip blanks *)
    |['\r'] ['\n']   {token lexbuf} (* skip blanks *)
    | ';'  { EOL }
    | ','  { COMMA }

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
    | "set" { SET }
    | "var" { VAR }
    | '"'      { STRDEL }
    | '('       { LPAREN }  | ')'       { RPAREN }
    | '{'       { LCURLY }  | '}'       { RCURLY }

    | ['0'-'9']+ as lxm { INT(int_of_string lxm) }
    | ['a'-'z''A'-'Z']+ as str { STRING(str) }

    | eof      { EOF }
    | '#'      { EOF }
