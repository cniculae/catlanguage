#!/bin/bash

ocamlc -c ParseTree.ml
ocamllex Lexer.mll
ocamlyacc Parser.mly
ocamlc -c Parser.mli
ocamlc -c Lexer.ml
ocamlc -c Parser.ml
ocamlc -c Interpreter.ml
ocamlc -o Main Lexer.cmo Parser.cmo Interpreter.cmo
