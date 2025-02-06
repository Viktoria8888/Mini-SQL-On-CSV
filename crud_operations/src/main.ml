open Ast
open Types
open Monad
open Query
let parse (s : string) : (command, errors) res =
  try
    let lexbuf = Lexing.from_string s in
    let ast = Parser.main Lexer.tokenize lexbuf in
    return ast
  with
  | _ -> Error (ParseError "failed to parse query")

