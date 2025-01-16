open Interp
open Ast
open Parser
open Lexer


let parse_string s =
  let lexbuf = Lexing.from_string s in
  try
    Some (main tokenize lexbuf)
  with
  | _ -> None

let test_parser () =
  let test_cases = [
    ("READ column1 FROM table1 WHERE column2 = 2", Some (Read ("column1", "table1", Some (Interp.Ast.Equal ("column2", "2")))));
    ("DELETE table1 WHERE column1 = 1", Some (Delete ("table1", Some (Interp.Ast.Equal ("column1", "1")))));
    ("SELECT * FROM table1 WHERE column1 > 1", Some (SelectAll ("table1", Some (Interp.Ast.Greater ("column1", "1")))));
  ] in
  List.iter (fun (input, expected) ->
    let result = parse_string input in
    if result = expected then
      Printf.printf "Test passed for input: %s\n" input
    else
      Printf.printf "Test failed for input: %s\n" input; 
  ) test_cases

let () = test_parser () 