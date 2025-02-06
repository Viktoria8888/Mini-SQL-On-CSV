open Ast
open Types

let parse (s : string) : (command, errors) res =
  try
    let lexbuf = Lexing.from_string s in
    let ast = Parser.main Lexer.tokenize lexbuf in
    Ok ast
  with
  | _ -> Error (ParseError "failed to parse query")

let check_table (name : string) : (string, errors) res =
  let file_path = Filename.concat "../csv_files/" name in
  if Sys.file_exists file_path then Ok file_path else Error (FileNotFound name)
