type parsetree =
|Integer of int
|String of string
|Boolean of bool
|CreateSet of parsetree

|SingleStatement of parsetree
|MultiStatements of parsetree*parsetree

|ExecuteIf of parsetree*parsetree
|ExecuteWhile of parsetree*parsetree

|Assign of parsetree*parsetree
|AssignExistingVariable of parsetree*parsetree
|AssignSet of parsetree*parsetree
|AssignEmptySet of parsetree
|AddToSet of parsetree*parsetree
|DeleteFromSet of parsetree*parsetree
|GetVariable of parsetree

|PlusEqual of parsetree*parsetree
|MinusEqual of parsetree*parsetree
|DivEqual of parsetree*parsetree
|MultEqual  of parsetree*parsetree

|NotOperator of parsetree
|AndOperator of parsetree*parsetree
|OrOperator of parsetree*parsetree
|IsBigger of parsetree*parsetree
|IsSmaller of parsetree*parsetree
|IsNotEqual of parsetree*parsetree
|IsEqual of parsetree*parsetree
|IsSmallerOrEqual of parsetree*parsetree
|IsBiggerOrEqual of parsetree*parsetree

|Sum of parsetree*parsetree
|Dif of parsetree*parsetree
|Mult of parsetree*parsetree
|Div of parsetree*parsetree
|Mod of parsetree*parsetree

|Concat of parsetree*parsetree

|Union of parsetree*parsetree
|Intersection of parsetree*parsetree
|SetDifference of parsetree*parsetree
|Length of parsetree
|Find of parsetree*parsetree

|ReadSet of parsetree
|ReadInt of parsetree

|Print of parsetree
|PrintNL of parsetree
|PrintSet of parsetree
