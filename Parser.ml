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

open Parsing;;
let _ = parse_error;;
# 1 "Parser.mly"

open ParseTree
# 62 "Parser.ml"
let yytransl_const = [|
  257 (* LCURLY *);
  258 (* RCURLY *);
  259 (* LPAREN *);
  260 (* RPAREN *);
  261 (* COMMA *);
  262 (* LEN *);
  263 (* FIND *);
  264 (* READINT *);
  265 (* READSET *);
  266 (* VAR *);
  269 (* ASSIGN *);
  270 (* MULT *);
  271 (* DIV *);
  272 (* PLUS *);
  273 (* DIF *);
  274 (* MOD *);
  275 (* PLUSEQ *);
  276 (* DIFEQ *);
  277 (* DIVEQ *);
  278 (* MULTEQ *);
  279 (* COMM *);
  280 (* NOTEQ *);
  281 (* SMALLER *);
  282 (* BIGGER *);
  283 (* SMOREQ *);
  284 (* BIGOREQ *);
  285 (* EQUAL *);
  286 (* NOT *);
  287 (* AND *);
  288 (* OR *);
  289 (* IF *);
  290 (* WHILE *);
  291 (* EOL *);
    0 (* EOF *);
  292 (* CONCAT *);
  293 (* TRUE *);
  294 (* FALSE *);
  295 (* STRDEL *);
  296 (* PRINTNL *);
  297 (* PRINT *);
  298 (* SET *);
  299 (* ADDTOSET *);
  300 (* CREATESET *);
  301 (* PRINTSET *);
  302 (* UNION *);
  303 (* INTER *);
  304 (* SETDIF *);
  305 (* EMPTYSET *);
  306 (* DELETEFROMSET *);
  307 (* BEGIN *);
  308 (* END *);
    0|]

let yytransl_block = [|
  267 (* INT *);
  268 (* STRING *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\001\000\001\000\002\000\002\000\003\000\003\000\
\003\000\003\000\003\000\005\000\005\000\005\000\005\000\005\000\
\005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
\005\000\005\000\005\000\005\000\005\000\004\000\004\000\004\000\
\004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
\004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
\004\000\004\000\004\000\004\000\004\000\004\000\004\000\000\000"

let yylen = "\002\000\
\002\000\005\000\004\000\001\000\003\000\001\000\005\000\005\000\
\002\000\003\000\002\000\004\000\004\000\004\000\004\000\003\000\
\004\000\004\000\006\000\006\000\006\000\006\000\006\000\004\000\
\004\000\003\000\003\000\003\000\003\000\001\000\001\000\001\000\
\002\000\003\000\001\000\003\000\003\000\003\000\003\000\003\000\
\003\000\002\000\002\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\002\000\004\000\006\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\004\000\000\000\056\000\001\000\000\000\
\006\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\005\000\
\000\000\000\000\000\000\003\000\000\000\000\000\000\000\000\000\
\000\000\000\000\030\000\035\000\000\000\000\000\000\000\031\000\
\032\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\002\000\024\000\025\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\033\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\013\000\
\012\000\018\000\000\000\000\000\014\000\000\000\000\000\000\000\
\000\000\036\000\000\000\000\000\034\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\007\000\008\000\000\000\000\000\000\000\
\000\000\000\000\054\000\000\000\019\000\021\000\022\000\023\000\
\020\000\000\000\055\000"

let yydgoto = "\002\000\
\006\000\010\000\049\000\067\000\027\000"

let yysindex = "\013\000\
\001\000\000\000\023\000\000\000\003\000\000\000\000\000\088\255\
\000\000\228\254\022\255\025\255\017\255\139\255\027\255\028\255\
\033\255\042\255\036\255\046\255\051\255\058\255\061\255\062\255\
\083\255\010\255\047\255\002\000\076\255\081\255\086\255\134\255\
\134\255\134\255\134\255\134\255\134\255\134\255\134\255\134\255\
\093\255\095\255\097\255\099\255\100\255\101\255\106\255\000\000\
\088\255\088\255\091\000\000\000\116\255\120\255\134\255\134\255\
\122\255\123\255\000\000\000\000\134\255\134\255\134\255\000\000\
\000\000\001\255\097\001\097\001\097\001\097\001\097\001\151\255\
\177\255\203\255\237\255\078\255\127\255\135\255\137\255\138\255\
\142\255\143\255\088\255\000\000\000\000\000\000\097\001\014\000\
\132\255\141\255\023\255\023\255\087\255\110\255\000\000\134\255\
\134\255\134\255\134\255\134\255\134\255\134\255\134\255\134\255\
\134\255\134\255\134\255\134\255\134\255\003\000\003\000\000\000\
\000\000\000\000\097\001\134\255\000\000\144\255\145\255\150\255\
\134\255\000\000\159\255\149\255\000\000\003\255\003\255\023\255\
\023\255\148\255\087\255\087\255\087\255\087\255\087\255\087\255\
\120\001\143\001\097\001\000\000\000\000\040\000\166\255\170\255\
\181\255\066\000\000\000\134\255\000\000\000\000\000\000\000\000\
\000\000\092\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\184\255\186\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\154\255\155\255\161\255\163\255\164\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\195\255\000\000\000\000\000\000\165\255\000\000\
\000\000\000\000\187\000\204\000\253\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\175\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\144\000\170\000\221\000\
\238\000\118\000\011\001\023\001\037\001\049\001\063\001\075\001\
\079\255\031\255\011\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\162\255\248\255\227\255\000\000"

let yytablesize = 691
let yytable = "\026\000\
\004\000\052\000\009\000\068\000\069\000\070\000\071\000\072\000\
\073\000\074\000\075\000\048\000\094\000\001\000\044\000\140\000\
\141\000\011\000\012\000\013\000\100\000\014\000\007\000\028\000\
\029\000\087\000\088\000\030\000\031\000\037\000\038\000\091\000\
\092\000\093\000\052\000\039\000\096\000\097\000\109\000\095\000\
\100\000\083\000\015\000\016\000\040\000\044\000\115\000\041\000\
\042\000\017\000\018\000\019\000\020\000\043\000\021\000\022\000\
\023\000\024\000\109\000\025\000\044\000\052\000\052\000\045\000\
\046\000\052\000\126\000\127\000\128\000\129\000\130\000\131\000\
\132\000\133\000\134\000\135\000\136\000\137\000\138\000\139\000\
\056\000\050\000\051\000\057\000\058\000\047\000\142\000\053\000\
\059\000\060\000\084\000\146\000\054\000\061\000\062\000\011\000\
\012\000\013\000\055\000\014\000\096\000\097\000\098\000\099\000\
\100\000\076\000\077\000\063\000\078\000\051\000\079\000\080\000\
\081\000\051\000\064\000\065\000\066\000\082\000\154\000\085\000\
\015\000\016\000\109\000\086\000\089\000\090\000\114\000\017\000\
\018\000\019\000\020\000\116\000\021\000\022\000\023\000\024\000\
\056\000\025\000\117\000\057\000\058\000\118\000\119\000\123\000\
\059\000\060\000\120\000\121\000\125\000\061\000\062\000\032\000\
\124\000\148\000\110\000\143\000\144\000\033\000\034\000\035\000\
\036\000\145\000\147\000\063\000\096\000\097\000\098\000\099\000\
\100\000\150\000\064\000\065\000\066\000\151\000\101\000\102\000\
\103\000\104\000\105\000\106\000\111\000\107\000\108\000\109\000\
\152\000\011\000\109\000\009\000\016\000\026\000\096\000\097\000\
\098\000\099\000\100\000\029\000\010\000\028\000\027\000\015\000\
\101\000\102\000\103\000\104\000\105\000\106\000\112\000\107\000\
\108\000\017\000\000\000\000\000\109\000\000\000\000\000\000\000\
\096\000\097\000\098\000\099\000\100\000\000\000\000\000\000\000\
\000\000\000\000\101\000\102\000\103\000\104\000\105\000\106\000\
\000\000\107\000\108\000\000\000\000\000\000\000\109\000\000\000\
\113\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\096\000\097\000\098\000\099\000\100\000\000\000\
\000\000\000\000\000\000\008\000\101\000\102\000\103\000\104\000\
\105\000\106\000\000\000\107\000\108\000\000\000\000\000\000\000\
\109\000\122\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\096\000\097\000\098\000\099\000\100\000\
\000\000\000\000\000\000\003\000\051\000\101\000\102\000\103\000\
\104\000\105\000\106\000\149\000\107\000\108\000\000\000\000\000\
\000\000\109\000\000\000\005\000\000\000\096\000\097\000\098\000\
\099\000\100\000\000\000\000\000\000\000\000\000\000\000\101\000\
\102\000\103\000\104\000\105\000\106\000\153\000\107\000\108\000\
\000\000\000\000\000\000\109\000\000\000\000\000\000\000\096\000\
\097\000\098\000\099\000\100\000\000\000\000\000\000\000\000\000\
\000\000\101\000\102\000\103\000\104\000\105\000\106\000\155\000\
\107\000\108\000\000\000\000\000\000\000\109\000\000\000\000\000\
\000\000\096\000\097\000\098\000\099\000\100\000\000\000\000\000\
\000\000\000\000\000\000\101\000\102\000\103\000\104\000\105\000\
\106\000\041\000\107\000\108\000\000\000\000\000\000\000\109\000\
\000\000\000\000\000\000\041\000\041\000\041\000\041\000\041\000\
\000\000\000\000\000\000\000\000\000\000\041\000\041\000\041\000\
\041\000\041\000\041\000\037\000\041\000\041\000\000\000\000\000\
\041\000\000\000\000\000\000\000\000\000\037\000\037\000\037\000\
\037\000\000\000\000\000\000\000\000\000\000\000\000\000\037\000\
\037\000\037\000\037\000\037\000\037\000\038\000\037\000\037\000\
\000\000\000\000\037\000\000\000\000\000\000\000\000\000\038\000\
\038\000\038\000\038\000\000\000\000\000\000\000\043\000\000\000\
\000\000\038\000\038\000\038\000\038\000\038\000\038\000\000\000\
\038\000\038\000\043\000\043\000\038\000\000\000\000\000\042\000\
\000\000\000\000\043\000\043\000\043\000\043\000\043\000\043\000\
\000\000\043\000\043\000\042\000\042\000\043\000\000\000\000\000\
\039\000\000\000\000\000\042\000\042\000\042\000\042\000\042\000\
\042\000\000\000\042\000\042\000\039\000\039\000\042\000\000\000\
\000\000\040\000\000\000\000\000\039\000\039\000\039\000\039\000\
\039\000\039\000\000\000\039\000\039\000\040\000\040\000\039\000\
\053\000\000\000\000\000\000\000\000\000\040\000\040\000\040\000\
\040\000\040\000\040\000\000\000\040\000\040\000\047\000\000\000\
\040\000\000\000\000\000\000\000\053\000\053\000\053\000\053\000\
\053\000\053\000\046\000\053\000\053\000\000\000\000\000\053\000\
\000\000\000\000\047\000\047\000\047\000\047\000\047\000\047\000\
\045\000\047\000\047\000\000\000\000\000\047\000\046\000\046\000\
\046\000\046\000\046\000\046\000\049\000\046\000\046\000\000\000\
\000\000\046\000\000\000\000\000\045\000\045\000\045\000\045\000\
\045\000\045\000\050\000\045\000\045\000\000\000\000\000\045\000\
\049\000\049\000\049\000\049\000\049\000\049\000\048\000\049\000\
\049\000\000\000\000\000\049\000\000\000\000\000\050\000\050\000\
\050\000\050\000\050\000\050\000\000\000\050\000\050\000\000\000\
\000\000\050\000\048\000\048\000\048\000\048\000\048\000\048\000\
\000\000\048\000\048\000\000\000\000\000\048\000\096\000\097\000\
\098\000\099\000\100\000\000\000\000\000\000\000\000\000\000\000\
\101\000\102\000\103\000\104\000\105\000\106\000\000\000\107\000\
\108\000\000\000\000\000\000\000\109\000\096\000\097\000\098\000\
\099\000\100\000\000\000\000\000\000\000\000\000\000\000\101\000\
\102\000\103\000\104\000\105\000\106\000\000\000\000\000\108\000\
\000\000\000\000\000\000\109\000\096\000\097\000\098\000\099\000\
\100\000\000\000\000\000\000\000\000\000\000\000\101\000\102\000\
\103\000\104\000\105\000\106\000\000\000\000\000\000\000\000\000\
\000\000\000\000\109\000"

let yycheck = "\008\000\
\000\000\000\000\000\000\033\000\034\000\035\000\036\000\037\000\
\038\000\039\000\040\000\002\001\012\001\001\000\004\001\110\000\
\111\000\008\001\009\001\010\001\018\001\012\001\000\000\052\001\
\003\001\055\000\056\000\003\001\012\001\003\001\003\001\061\000\
\062\000\063\000\004\001\003\001\014\001\015\001\036\001\039\001\
\018\001\050\000\033\001\034\001\003\001\035\001\076\000\012\001\
\003\001\040\001\041\001\042\001\043\001\003\001\045\001\046\001\
\047\001\048\001\036\001\050\001\003\001\031\001\032\001\003\001\
\003\001\035\001\096\000\097\000\098\000\099\000\100\000\101\000\
\102\000\103\000\104\000\105\000\106\000\107\000\108\000\109\000\
\003\001\035\001\004\001\006\001\007\001\003\001\116\000\012\001\
\011\001\012\001\000\000\121\000\012\001\016\001\017\001\008\001\
\009\001\010\001\013\001\012\001\014\001\015\001\016\001\017\001\
\018\001\013\001\012\001\030\001\012\001\031\001\012\001\012\001\
\012\001\035\001\037\001\038\001\039\001\012\001\148\000\004\001\
\033\001\034\001\036\001\004\001\003\001\003\001\049\001\040\001\
\041\001\042\001\043\001\005\001\045\001\046\001\047\001\048\001\
\003\001\050\001\004\001\006\001\007\001\005\001\005\001\012\001\
\011\001\012\001\005\001\005\001\039\001\016\001\017\001\013\001\
\012\001\005\001\004\001\012\001\012\001\019\001\020\001\021\001\
\022\001\012\001\004\001\030\001\014\001\015\001\016\001\017\001\
\018\001\004\001\037\001\038\001\039\001\004\001\024\001\025\001\
\026\001\027\001\028\001\029\001\004\001\031\001\032\001\036\001\
\004\001\002\001\036\001\002\001\035\001\035\001\014\001\015\001\
\016\001\017\001\018\001\035\001\002\001\035\001\035\001\035\001\
\024\001\025\001\026\001\027\001\028\001\029\001\004\001\031\001\
\032\001\035\001\255\255\255\255\036\001\255\255\255\255\255\255\
\014\001\015\001\016\001\017\001\018\001\255\255\255\255\255\255\
\255\255\255\255\024\001\025\001\026\001\027\001\028\001\029\001\
\255\255\031\001\032\001\255\255\255\255\255\255\036\001\255\255\
\004\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\014\001\015\001\016\001\017\001\018\001\255\255\
\255\255\255\255\255\255\001\001\024\001\025\001\026\001\027\001\
\028\001\029\001\255\255\031\001\032\001\255\255\255\255\255\255\
\036\001\004\001\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\014\001\015\001\016\001\017\001\018\001\
\255\255\255\255\255\255\035\001\035\001\024\001\025\001\026\001\
\027\001\028\001\029\001\004\001\031\001\032\001\255\255\255\255\
\255\255\036\001\255\255\051\001\255\255\014\001\015\001\016\001\
\017\001\018\001\255\255\255\255\255\255\255\255\255\255\024\001\
\025\001\026\001\027\001\028\001\029\001\004\001\031\001\032\001\
\255\255\255\255\255\255\036\001\255\255\255\255\255\255\014\001\
\015\001\016\001\017\001\018\001\255\255\255\255\255\255\255\255\
\255\255\024\001\025\001\026\001\027\001\028\001\029\001\004\001\
\031\001\032\001\255\255\255\255\255\255\036\001\255\255\255\255\
\255\255\014\001\015\001\016\001\017\001\018\001\255\255\255\255\
\255\255\255\255\255\255\024\001\025\001\026\001\027\001\028\001\
\029\001\004\001\031\001\032\001\255\255\255\255\255\255\036\001\
\255\255\255\255\255\255\014\001\015\001\016\001\017\001\018\001\
\255\255\255\255\255\255\255\255\255\255\024\001\025\001\026\001\
\027\001\028\001\029\001\004\001\031\001\032\001\255\255\255\255\
\035\001\255\255\255\255\255\255\255\255\014\001\015\001\016\001\
\017\001\255\255\255\255\255\255\255\255\255\255\255\255\024\001\
\025\001\026\001\027\001\028\001\029\001\004\001\031\001\032\001\
\255\255\255\255\035\001\255\255\255\255\255\255\255\255\014\001\
\015\001\016\001\017\001\255\255\255\255\255\255\004\001\255\255\
\255\255\024\001\025\001\026\001\027\001\028\001\029\001\255\255\
\031\001\032\001\016\001\017\001\035\001\255\255\255\255\004\001\
\255\255\255\255\024\001\025\001\026\001\027\001\028\001\029\001\
\255\255\031\001\032\001\016\001\017\001\035\001\255\255\255\255\
\004\001\255\255\255\255\024\001\025\001\026\001\027\001\028\001\
\029\001\255\255\031\001\032\001\016\001\017\001\035\001\255\255\
\255\255\004\001\255\255\255\255\024\001\025\001\026\001\027\001\
\028\001\029\001\255\255\031\001\032\001\016\001\017\001\035\001\
\004\001\255\255\255\255\255\255\255\255\024\001\025\001\026\001\
\027\001\028\001\029\001\255\255\031\001\032\001\004\001\255\255\
\035\001\255\255\255\255\255\255\024\001\025\001\026\001\027\001\
\028\001\029\001\004\001\031\001\032\001\255\255\255\255\035\001\
\255\255\255\255\024\001\025\001\026\001\027\001\028\001\029\001\
\004\001\031\001\032\001\255\255\255\255\035\001\024\001\025\001\
\026\001\027\001\028\001\029\001\004\001\031\001\032\001\255\255\
\255\255\035\001\255\255\255\255\024\001\025\001\026\001\027\001\
\028\001\029\001\004\001\031\001\032\001\255\255\255\255\035\001\
\024\001\025\001\026\001\027\001\028\001\029\001\004\001\031\001\
\032\001\255\255\255\255\035\001\255\255\255\255\024\001\025\001\
\026\001\027\001\028\001\029\001\255\255\031\001\032\001\255\255\
\255\255\035\001\024\001\025\001\026\001\027\001\028\001\029\001\
\255\255\031\001\032\001\255\255\255\255\035\001\014\001\015\001\
\016\001\017\001\018\001\255\255\255\255\255\255\255\255\255\255\
\024\001\025\001\026\001\027\001\028\001\029\001\255\255\031\001\
\032\001\255\255\255\255\255\255\036\001\014\001\015\001\016\001\
\017\001\018\001\255\255\255\255\255\255\255\255\255\255\024\001\
\025\001\026\001\027\001\028\001\029\001\255\255\255\255\032\001\
\255\255\255\255\255\255\036\001\014\001\015\001\016\001\017\001\
\018\001\255\255\255\255\255\255\255\255\255\255\024\001\025\001\
\026\001\027\001\028\001\029\001\255\255\255\255\255\255\255\255\
\255\255\255\255\036\001"

let yynames_const = "\
  LCURLY\000\
  RCURLY\000\
  LPAREN\000\
  RPAREN\000\
  COMMA\000\
  LEN\000\
  FIND\000\
  READINT\000\
  READSET\000\
  VAR\000\
  ASSIGN\000\
  MULT\000\
  DIV\000\
  PLUS\000\
  DIF\000\
  MOD\000\
  PLUSEQ\000\
  DIFEQ\000\
  DIVEQ\000\
  MULTEQ\000\
  COMM\000\
  NOTEQ\000\
  SMALLER\000\
  BIGGER\000\
  SMOREQ\000\
  BIGOREQ\000\
  EQUAL\000\
  NOT\000\
  AND\000\
  OR\000\
  IF\000\
  WHILE\000\
  EOL\000\
  EOF\000\
  CONCAT\000\
  TRUE\000\
  FALSE\000\
  STRDEL\000\
  PRINTNL\000\
  PRINT\000\
  SET\000\
  ADDTOSET\000\
  CREATESET\000\
  PRINTSET\000\
  UNION\000\
  INTER\000\
  SETDIF\000\
  EMPTYSET\000\
  DELETEFROMSET\000\
  BEGIN\000\
  END\000\
  "

let yynames_block = "\
  INT\000\
  STRING\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    Obj.repr(
# 49 "Parser.mly"
                        ( Integer(0) )
# 456 "Parser.ml"
               : ParseTree.parsetree))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'scope) in
    Obj.repr(
# 50 "Parser.mly"
                                  ( _2 )
# 463 "Parser.ml"
               : ParseTree.parsetree))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'scope) in
    Obj.repr(
# 51 "Parser.mly"
                                  ( _2 )
# 470 "Parser.ml"
               : ParseTree.parsetree))
; (fun __caml_parser_env ->
    Obj.repr(
# 52 "Parser.mly"
                        ( Integer(0) )
# 476 "Parser.ml"
               : ParseTree.parsetree))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'statements) in
    Obj.repr(
# 56 "Parser.mly"
                                ( _2 )
# 483 "Parser.ml"
               : 'scope))
; (fun __caml_parser_env ->
    Obj.repr(
# 57 "Parser.mly"
                                ( Integer(0) )
# 489 "Parser.ml"
               : 'scope))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'scope) in
    Obj.repr(
# 61 "Parser.mly"
                                        ( ExecuteIf(_3,_5) )
# 497 "Parser.ml"
               : 'statements))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'scope) in
    Obj.repr(
# 62 "Parser.mly"
                                        ( ExecuteWhile(_3,_5) )
# 505 "Parser.ml"
               : 'statements))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'call) in
    Obj.repr(
# 63 "Parser.mly"
                                        ( SingleStatement(_1) )
# 512 "Parser.ml"
               : 'statements))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'call) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'statements) in
    Obj.repr(
# 64 "Parser.mly"
                                        ( MultiStatements(_1,_3) )
# 520 "Parser.ml"
               : 'statements))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'statements) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'statements) in
    Obj.repr(
# 65 "Parser.mly"
                                        ( MultiStatements(_1,_2) )
# 528 "Parser.ml"
               : 'statements))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 69 "Parser.mly"
                                        ( Print(_3) )
# 535 "Parser.ml"
               : 'call))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 70 "Parser.mly"
                                        ( PrintNL(_3) )
# 542 "Parser.ml"
               : 'call))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 71 "Parser.mly"
                                        ( PrintSet(String(_3)) )
# 549 "Parser.ml"
               : 'call))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 72 "Parser.mly"
                                        ( Assign(String(_2),_4) )
# 557 "Parser.ml"
               : 'call))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 73 "Parser.mly"
                                        ( AssignExistingVariable(String(_1),_3) )
# 565 "Parser.ml"
               : 'call))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 74 "Parser.mly"
                                        ( AssignSet(String(_2),_4) )
# 573 "Parser.ml"
               : 'call))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    Obj.repr(
# 75 "Parser.mly"
                                        ( AssignEmptySet(String(_2)) )
# 580 "Parser.ml"
               : 'call))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 76 "Parser.mly"
                                                           ( AddToSet(String(_3),_5) )
# 588 "Parser.ml"
               : 'call))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 77 "Parser.mly"
                                                           ( DeleteFromSet(String(_3),_5) )
# 596 "Parser.ml"
               : 'call))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 78 "Parser.mly"
                                                           ( Union(String(_3),String(_5)) )
# 604 "Parser.ml"
               : 'call))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 79 "Parser.mly"
                                                           ( Intersection(String(_3),String(_5)) )
# 612 "Parser.ml"
               : 'call))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 80 "Parser.mly"
                                                           ( SetDifference(String(_3),String(_5)) )
# 620 "Parser.ml"
               : 'call))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 81 "Parser.mly"
                                        ( ReadInt(String(_3)) )
# 627 "Parser.ml"
               : 'call))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 82 "Parser.mly"
                                        ( ReadSet(String(_3)) )
# 634 "Parser.ml"
               : 'call))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 83 "Parser.mly"
                                        ( PlusEqual(String(_1),_3) )
# 642 "Parser.ml"
               : 'call))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 84 "Parser.mly"
                                        ( MultEqual(String(_1),_3) )
# 650 "Parser.ml"
               : 'call))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 85 "Parser.mly"
                                       ( DivEqual(String(_1),_3) )
# 658 "Parser.ml"
               : 'call))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 86 "Parser.mly"
                                       ( MinusEqual(String(_1),_3) )
# 666 "Parser.ml"
               : 'call))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 91 "Parser.mly"
                            ( Integer(_1) )
# 673 "Parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 92 "Parser.mly"
                            ( Boolean(true) )
# 679 "Parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 93 "Parser.mly"
                            ( Boolean(false) )
# 685 "Parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 94 "Parser.mly"
                            ( String("") )
# 691 "Parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 95 "Parser.mly"
                            ( String(_2) )
# 698 "Parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 96 "Parser.mly"
                            ( GetVariable(String(_1)) )
# 705 "Parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 98 "Parser.mly"
                            ( _2 )
# 712 "Parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 99 "Parser.mly"
                        ( Mult(_1,_3) )
# 720 "Parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 99 "Parser.mly"
                                                            ( Div(_1,_3) )
# 728 "Parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 100 "Parser.mly"
                        ( Sum(_1,_3) )
# 736 "Parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 100 "Parser.mly"
                                                            ( Dif(_1,_3) )
# 744 "Parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 101 "Parser.mly"
                        ( Mod(_1,_3) )
# 752 "Parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 102 "Parser.mly"
                        ( Dif(Integer(0),_2) )
# 759 "Parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 102 "Parser.mly"
                                                                   ( Sum(Integer(0),_2) )
# 766 "Parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 104 "Parser.mly"
                        ( Concat(_1,_3) )
# 774 "Parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 106 "Parser.mly"
                        (IsBigger(_1,_3))
# 782 "Parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 106 "Parser.mly"
                                                              (IsSmaller(_1,_3))
# 790 "Parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 107 "Parser.mly"
                      (IsNotEqual(_1,_3))
# 798 "Parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 107 "Parser.mly"
                                                            (IsEqual(_1,_3))
# 806 "Parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 108 "Parser.mly"
                       (IsSmallerOrEqual(_1,_3))
# 814 "Parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 108 "Parser.mly"
                                                                     (IsBiggerOrEqual(_1,_3))
# 822 "Parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 110 "Parser.mly"
                    ( AndOperator(_1,_3) )
# 830 "Parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 110 "Parser.mly"
                                                          ( OrOperator(_1,_3) )
# 838 "Parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 111 "Parser.mly"
                    ( NotOperator(_2) )
# 845 "Parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 113 "Parser.mly"
                                        ( Length(String(_3)) )
# 852 "Parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 114 "Parser.mly"
                                                   ( Find(String(_3),_5) )
# 860 "Parser.ml"
               : 'expr))
(* Entry main *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let main (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : ParseTree.parsetree)
