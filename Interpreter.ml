(* File interpreter.ml *)
open Str;;
open String;;
open List;;
open Set;;
open ParseTree;;
open Lexer;;
exception Runtime_error
exception Illegal_argument
exception LoopBreak
exception CloseProgram

(* Initialize Variables *)
module SS = Set.Make(String);;

let stringVariables : (string, string) Hashtbl.t = Hashtbl.create 3000;;
let intVariables : (string, int) Hashtbl.t = Hashtbl.create 3000;;
let boolVariables : (string, bool) Hashtbl.t = Hashtbl.create 3000;;
let setVariables : (string, SS.t) Hashtbl.t = Hashtbl.create 3000;;
let typeVariables : (string, char) Hashtbl.t = Hashtbl.create 3000;;

(* Helper Functions *)

let string_of_bool b = if b then "true" else "false";;

let createBool b =
  match b with
    | Integer int1 -> if int1 != 0 then (true) else (false)
    | String s1 -> if s1 != "0" && s1 != "" then (true) else (false)
    | Boolean b -> if b then (true) else (false)
    | _ -> raise Illegal_argument
;;

let rec printList = function
    | [] -> print_string ""
    | [x] -> print_string x
    | x::t -> print_string (x^", "); printList t
;;

let rec findList ls index =
  match ls,index with
    | [x], i -> if(i<=0) then x else raise Illegal_argument
    | x::t, i -> if(i<=0) then x else findList t (i-1)
    | _ -> raise Illegal_argument
;;


(* Parse Function *)
let rec parseCall =
(* Data Types: *)
 function
    |Integer i -> Integer i
    |String s -> String s
    |Boolean b -> Boolean b

(* Execute Statements *)
    |SingleStatement (s) -> parseCall s;
    |MultiStatements (s1,s2) -> parseCall s1; parseCall s2;

(* Assign Statements *)
    |Assign (name,value) ->
      (
        match (parseCall name),(parseCall value) with
          | String s1, String s2 -> Hashtbl.add stringVariables s1 s2; Hashtbl.add typeVariables s1 's'; Integer(0)
          | String s1, Integer int2 -> Hashtbl.add intVariables s1 int2; Hashtbl.add typeVariables s1 'i'; Integer(0)
          | String s1, Boolean b2 -> Hashtbl.add boolVariables s1 b2; Hashtbl.add typeVariables s1 'b'; Integer(0)
          | _ -> raise Illegal_argument
      )
    |AssignExistingVariable (name,value) ->
      (
          match (parseCall name) with
            | String s1 ->  (
                              match (Hashtbl.find typeVariables s1),(parseCall value) with
                                | 's' ,String s2 -> Hashtbl.add stringVariables s1 s2; Hashtbl.add typeVariables s1 's'; Integer(0)
                                | 'i' ,Integer int2 -> Hashtbl.add intVariables s1 int2; Hashtbl.add typeVariables s1 'i'; Integer(0)
                                | 'b' ,Boolean b2 ->  Hashtbl.add boolVariables s1 b2; Hashtbl.add typeVariables s1 'b'; Integer(0)
                                | _ -> raise Runtime_error
                            )
            | _ -> raise Illegal_argument
      )
    |AssignSet (name,initialValue) ->
      (
        match (parseCall name),(parseCall initialValue) with
          | String s1, String s2 -> Hashtbl.add setVariables s1 (SS.singleton s2); Hashtbl.add typeVariables s1 'l'; Integer(0)
          | String s1, Integer int1 -> Hashtbl.add setVariables s1 (SS.singleton (string_of_int int1)); Hashtbl.add typeVariables s1 'l'; Integer(0)
          | String s1, Boolean b1 -> Hashtbl.add setVariables s1 (SS.singleton (string_of_bool b1)); Hashtbl.add typeVariables s1 'l'; Integer(0)
          | _ -> raise Illegal_argument
      )
    |AssignEmptySet (name) ->
      (
        match (parseCall name) with
          | String s1 -> Hashtbl.add setVariables s1 (SS.empty); Hashtbl.add typeVariables s1 'l'; Integer(0)
          | _ -> raise Illegal_argument
      )
    |AddToSet (name,valueToAdd) ->
      (
        match(parseCall name),(parseCall valueToAdd) with
          | String s1, String s2 -> Hashtbl.add setVariables s1 (SS.add s2 (Hashtbl.find setVariables s1)); Integer(0)
          | String s1, Integer int2 -> Hashtbl.add setVariables s1 (SS.add (string_of_int int2) (Hashtbl.find setVariables s1)); Integer(0)
          | String s1, Boolean b2 -> Hashtbl.add setVariables s1 (SS.add (string_of_bool b2) (Hashtbl.find setVariables s1)); Integer(0)
          | _ -> raise Illegal_argument
      )
    |GetVariable (name) ->
      (
        match (parseCall name) with
          | String s1 ->
              (
                match (Hashtbl.find typeVariables s1) with
                | 's' -> String(Hashtbl.find stringVariables s1)
                | 'i' -> Integer(Hashtbl.find intVariables s1)
                | 'b' -> Boolean(Hashtbl.find boolVariables s1)
              )
          | _ -> raise Illegal_argument
      )
    |PlusEqual (name, valueToAdd) ->
      (
        parseCall(AssignExistingVariable(name,Sum(GetVariable(name),valueToAdd)));
      )
    |MinusEqual (name, valueToSubstract) ->
      (
        parseCall(AssignExistingVariable(name,Dif(GetVariable(name),valueToSubstract)));
      )
    |DivEqual (name, valueToDivide) ->
      (
        parseCall(AssignExistingVariable(name,Div(GetVariable(name),valueToDivide)));
      )
    |MultEqual (name, valueToMultiply) ->
      (
        parseCall(AssignExistingVariable(name,Mult(GetVariable(name),valueToMultiply)));
      )

(* Logic Operators: *)
    |AndOperator (b1,b2) -> if ((createBool (parseCall b1)) && (createBool (parseCall b2))) then (Boolean true) else (Boolean false)
    |OrOperator (b1,b2) -> if ((createBool (parseCall b1)) || (createBool (parseCall b2))) then (Boolean true) else (Boolean false)
    |NotOperator (b1) -> if (createBool (parseCall b1)) then (Boolean false) else (Boolean true)
    |IsBigger (b1,b2) -> Boolean ((parseCall b1)>(parseCall b2))
    |IsSmaller (b1,b2) -> Boolean ((parseCall b1)<(parseCall b2))
    |IsBiggerOrEqual (b1,b2) -> Boolean ((parseCall b1)>=(parseCall b2))
    |IsSmallerOrEqual (b1,b2) -> Boolean ((parseCall b1)<=(parseCall b2))
    |IsNotEqual (b1,b2) -> Boolean ((parseCall b1)<>(parseCall b2))
    |IsEqual (b1,b2) -> Boolean ((parseCall b1)=(parseCall b2))

(* Basic Math Operations: *)

    |Sum (a,b) ->
  		(
  			match (parseCall a), (parseCall b) with
  				| Integer int1, Integer int2 -> Integer (int1 + int2)
          | _ -> raise Illegal_argument
  		)
    |Dif (a,b) ->
  		(
  			match (parseCall a), (parseCall b) with
  				| Integer int1, Integer int2 -> Integer (int1 - int2)
          | _ -> raise Illegal_argument
  		)
    |Mult (a,b) ->
      (
        match (parseCall a), (parseCall b) with
          | Integer int1, Integer int2 -> Integer (int1 * int2)
          | _ -> raise Illegal_argument
      )
    |Div (a,b) ->
      (
        match (parseCall a), (parseCall b) with
          | Integer int1, Integer int2 -> Integer (int1 / int2)
          | _ -> raise Illegal_argument
      )
    |Mod (a,b) ->
      (
        match (parseCall a), (parseCall b) with
          | Integer int1, Integer int2 -> Integer (int1 mod int2)
          | _ -> raise Illegal_argument
      )

(* String Operations: *)
    |Concat (a,b) ->
      (
        match (parseCall a), (parseCall b) with
          | Integer int1, Integer int2 -> String ((string_of_int int1) ^ (string_of_int int2))
          | String s1, Integer int2 -> String (s1 ^ (string_of_int int2))
          | Integer int1, String s2 -> String ((string_of_int int1) ^ s2)
          | String s1, String s2 -> String (s1^s2)
          | _ -> raise Illegal_argument
      )

(* Set Operations: *)
    |Union (a,b) ->
      (
        match (parseCall a),(parseCall b) with
          | String s1, String s2-> if ((Hashtbl.find typeVariables s1) == 'l') && ((Hashtbl.find typeVariables s2) == 'l')
            then
            (
              Hashtbl.add setVariables s1 (SS.union (Hashtbl.find setVariables s1) (Hashtbl.find setVariables s2));
              Integer(0);
            )
            else raise Illegal_argument
          | _ -> raise Illegal_argument
      )
    |Intersection (a,b) ->
      (
        match (parseCall a),(parseCall b) with
          | String s1, String s2-> if ((Hashtbl.find typeVariables s1) == 'l') && ((Hashtbl.find typeVariables s2) == 'l')
            then
            (
              Hashtbl.add setVariables s1 (SS.inter (Hashtbl.find setVariables s1) (Hashtbl.find setVariables s2));
              Integer(0);
            )
            else raise Illegal_argument
          | _ -> raise Illegal_argument
      )
    |Length (s) ->
      (
        match (parseCall s) with
          | String s1 -> if ((Hashtbl.find typeVariables s1) == 'l') then Integer(SS.cardinal (Hashtbl.find setVariables s1)) else raise Illegal_argument
          | _ -> raise Illegal_argument
      )
    |Find(s,i)  ->
      (
        match (parseCall s),(parseCall i) with
          | String s1, Integer int2 -> if ((Hashtbl.find typeVariables s1) == 'l') then
          (
            String(findList (SS.elements (Hashtbl.find setVariables s1)) int2)
          )
          else raise Illegal_argument
          | _ -> raise Illegal_argument
      )
    |SetDifference(set1,set2) ->
      (
        match (parseCall set1),(parseCall set2) with
          | String s1, String s2 -> if((Hashtbl.find typeVariables s1) == 'l' && (Hashtbl.find typeVariables s2) == 'l')
          then
            (
                Hashtbl.add setVariables s1 (SS.diff (Hashtbl.find setVariables s1) (Hashtbl.find setVariables s2));
                Integer (0);
            )
          else raise Illegal_argument
          | _ -> raise Illegal_argument
      )
(* IFs and Loops *)
    |ExecuteIf (checkStm, execStm) ->
          if ((createBool (parseCall checkStm)) == true) then (parseCall execStm) else Integer(0);
    |ExecuteWhile (checkStm, execStm) ->
          while ((createBool (parseCall checkStm)) == true) do (parseCall execStm) done; Integer(0)

(* Printing Functions: *)

    |Print (toBePrinted) ->
  		(
  			match (parseCall toBePrinted) with
          | String s1 -> print_string s1; Integer (0)
          | Integer int1 -> print_string (string_of_int int1); Integer (0)
          | Boolean b1 -> print_string (string_of_bool b1); Integer (0)
          | _ -> raise Illegal_argument
  		)
    |PrintNL (toBePrinted) -> print_string "\n"; parseCall(Print(toBePrinted));
    |PrintSet (setToBePrinted) ->
      (
          match (parseCall setToBePrinted) with
            | String s1 -> if (Hashtbl.find typeVariables s1) == 'l'
              then
                (
                  print_string "{";
                  printList (SS.elements (Hashtbl.find setVariables s1));
                  print_string "}\n";
                  Integer(0);
                )
              else raise Illegal_argument
            | _ -> raise Illegal_argument
      )
(* Reading Functions *)
    |ReadInt (variable) ->
      (
          match (parseCall variable) with
            | String s1 -> parseCall(Assign(String(s1),Integer(read_int())));Integer(0)
            | _ -> raise Illegal_argument
      )
    |ReadSet (variable) ->
      (
          match (parseCall variable) with
            | String s1 ->
              (
                Hashtbl.add typeVariables s1 'l';
                Hashtbl.add setVariables s1 (SS.empty);
                let input = read_line() in
                let elementsList = if(input="{}") then [] else Str.split (regexp ", ") (List.hd (Str.split (regexp "}") (List.hd (Str.split (regexp "{") input)))) in
                  let rec putInSet ls set =
                    match ls with
                      | [x] -> if(x<"a") then parseCall(AddToSet(String(set),String(""))) else parseCall(AddToSet(String(set),String(x)))
                      | x::t -> if(x<"a") then parseCall(AddToSet(String(set),String(""))) else parseCall(AddToSet(String(set),String(x))); putInSet t set
                      | []  ->  parseCall(AssignEmptySet(String(set)))
                      | _ -> raise Illegal_argument
                  in
                    putInSet elementsList s1;
                Integer(0);
              )
            | _ -> raise Illegal_argument
      )
;;

(*Modify it and comment it: *)
let cin = open_in Sys.argv.(1);;
let lexbuf = Lexing.from_channel cin

let result = Parser.main Lexer.token lexbuf

let main () =
try
       parseCall result;
  with End_of_file -> exit 0
;;
(* START MAIN *)

main();;
