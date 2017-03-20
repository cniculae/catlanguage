%{
open ParseTree
%}
%token LCURLY RCURLY
%token LPAREN RPAREN
%token COMMA
%token LEN FIND
%token READINT READSET
%token VAR
%token <int> INT
%token <string> STRING
%token ASSIGN
%token MULT DIV PLUS DIF MOD
%token PLUSEQ DIFEQ DIVEQ MULTEQ

%token NOTEQ SMALLER BIGGER SMOREQ BIGOREQ EQUAL NOT
%token AND OR
%token IF WHILE
%token EOL EOF
%token CONCAT
%token TRUE FALSE
%token STRDEL
%token PRINTNL
%token PRINT
%token SET
%token ADDTOSET
%token CREATESET
%token PRINTSET
%token UNION INTER SETDIF
%token EMPTYSET

%left AND
%left OR
%left NOTEQ SMALLER BIGGER SMOREQ BIGOREQ EQUAL NOT      /* lowest precedence */
%left PLUS DIF        /* lowest precedence */
%left MULT DIV         /* medium precedence */
%left MOD

%type <ParseTree.parsetree> main


%start main             /* the entry point */

%%
/* PROGRAM: */
main:
    | EOL EOF           { Integer(0) }
    | scope EOL EOF     { $1 }
    | scope EOF         { $1 }
    | EOF               { Integer(0) }
;;
/* SOLVE IT LATER */
scope:
    | LCURLY statements RCURLY  { $2 }
    | EOF                       { Integer(0) }
;;

statements:
    | IF LPAREN expr RPAREN scope       { ExecuteIf($3,$5) }
    | WHILE LPAREN expr RPAREN scope    { ExecuteWhile($3,$5) }
    | call EOL                          { SingleStatement($1) }
    | call EOL statements               { MultiStatements($1,$3) }
    | statements statements             { MultiStatements($1,$2) }
;;

call:
    | PRINT LPAREN expr RPAREN          { Print($3) }
    | PRINTNL LPAREN expr RPAREN        { PrintNL($3) }
    | PRINTSET LPAREN STRING RPAREN     { PrintSet(String($3)) }
    | VAR STRING ASSIGN expr            { Assign(String($2),$4) }
    | STRING ASSIGN expr                { AssignExistingVariable(String($1),$3) }
    | SET STRING ASSIGN expr            { AssignSet(String($2),$4) }
    | SET STRING ASSIGN EMPTYSET        { AssignEmptySet(String($2)) }
    | ADDTOSET LPAREN STRING COMMA expr RPAREN             { AddToSet(String($3),$5) }
    | UNION LPAREN STRING COMMA STRING RPAREN              { Union(String($3),String($5)) }
    | INTER LPAREN STRING COMMA STRING RPAREN              { Intersection(String($3),String($5)) }
    | SETDIF LPAREN STRING COMMA STRING RPAREN             { SetDifference(String($3),String($5)) }
    | READINT LPAREN STRING RPAREN      { ReadInt(String($3)) }
    | READSET LPAREN STRING RPAREN      { ReadSet(String($3)) }
    | STRING PLUSEQ expr                { PlusEqual(String($1),$3) }
    | STRING MULTEQ expr                { MultEqual(String($1),$3) }
    | STRING DIVEQ expr                { DivEqual(String($1),$3) }
    | STRING DIFEQ expr                { MinusEqual(String($1),$3) }

;;

expr:
    | INT                   { Integer($1) }
    | TRUE                  { Boolean(true) }
    | FALSE                 { Boolean(false) }
    | STRDEL STRDEL         { String("") }
    | STRDEL STRING STRDEL  { String($2) }
    | STRING                { GetVariable(String($1)) }

    | LPAREN expr RPAREN    { $2 }
    | expr MULT expr    { Mult($1,$3) } | expr DIV expr     { Div($1,$3) }
    | expr PLUS expr    { Sum($1,$3) }  | expr DIF expr     { Dif($1,$3) }
    | expr MOD expr     { Mod($1,$3) }
    | DIF expr          { Dif(Integer(0),$2) } | PLUS expr         { Sum(Integer(0),$2) }

    | expr CONCAT expr  { Concat($1,$3) }

    | expr BIGGER expr  {IsBigger($1,$3)} | expr SMALLER expr {IsSmaller($1,$3)}
    | expr NOTEQ expr {IsNotEqual($1,$3)} | expr EQUAL expr {IsEqual($1,$3)}
    | expr SMOREQ expr {IsSmallerOrEqual($1,$3)} | expr BIGOREQ expr {IsBiggerOrEqual($1,$3)}

    | expr AND expr { AndOperator($1,$3) } | expr OR expr { OrOperator($1,$3) }
    | NOT expr      { NotOperator($2) }

    | LEN LPAREN STRING RPAREN          { Length(String($3)) }
    | FIND LPAREN STRING COMMA expr RPAREN         { Find(String($3),$5) }
;;
