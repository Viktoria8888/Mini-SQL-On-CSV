open Query_engine
open Data_manipulation
open Types
open Ast
open Query
open Monad 
let csv_data : (string, column) Hashtbl.t = Hashtbl.create 100

let check_file (filename: string) : (string, errors) res =
  let file_path = Filename.concat "csv_files/" filename in
  if Sys.file_exists file_path then (
    Printf.printf "File found: %s\n" file_path;
    Ok file_path
  ) else (
    Printf.printf "File not found: %s\n" file_path;
    Error (FileNotFound filename)
  )

let print_help () =
  Printf.printf "Available commands:\n";
  Printf.printf "  READ <column> FROM <file> [WHERE <column> = <value>]\n";
  Printf.printf "  SELECT * FROM <file> [WHERE <column> > <value>]\n";
  Printf.printf "  DELETE FROM <file> [WHERE <column> = <value>]\n";
  Printf.printf "  EXIT\n"

let parse (s : string) : (command, errors) res =
  try
    let lexbuf = Lexing.from_string s in
    let ast = Parser.main Lexer.tokenize lexbuf in
    return ast
  with
  | _ -> Error (ParseError "failed to parse query")

let process_query (filename: string) (query:string) = 
  let* cmd = (parse query) in
  let* file_path = (check_file filename) in
  let* _ = (read_csv_file file_path csv_data) in
  let res = (eval cmd csv_data) in res 

let rec repl (filename: string) () =
  Printf.printf "\nEnter query (or 'help', 'exit')> %!";
  match read_line () with
  | exception End_of_file -> ()
  | "exit" | "quit" -> ()
  | "help" ->
      print_help ();
      repl filename ()
  | query ->
      (match process_query filename query with
      | Ok result -> (
          match result with
          | Left res -> 
              Print_table.print_cols_data res.columns res.data;
              Printf.printf "Query executed successfully.\n%!"
          | Right msg -> 
              Printf.printf "%s\n%!" msg;
              Printf.printf "Query executed successfully.\n%!"
        )
      | Error e ->
          match e with
          | ParseError msg ->
              Printf.printf "Parse error: %s\n%!" msg
          | FileNotFound f ->
              Printf.printf "File not found: %s\n%!" f
          | InvalidCondition ->
              Printf.printf "Invalid condition in query.\n%!"
          | ColumnNotFound col ->
              Printf.printf "Column not found: %s\n%!" col
          | EmptyFile ->
              Printf.printf "Empty file.\n%!"
          | ParseCsvError msg ->  
              Printf.printf "CSV parse error: %s\n%!" msg);
      repl filename ()

let () =
if Array.length Sys.argv < 2 then
  Printf.printf "Usage: %s <filename>\n" Sys.argv.(0)
else
  let filename = Sys.argv.(1) in
  Printf.printf "CSV Query Engine\n";
  Printf.printf "Type 'help' for available commands\n";
  repl filename ()
  


