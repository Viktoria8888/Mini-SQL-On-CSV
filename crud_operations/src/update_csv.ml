
open Types

(* let parse (s : string) : (command, errors) res =
  try
    let lexbuf = Lexing.from_string s in
    let ast = Parser.main Lexer.tokenize lexbuf in
    Ok ast
  with
  | _ -> Error (ParseError "failed to parse query")
 *)


 let write_csv_file (filepath: string) (table: (string, column) Hashtbl.t) : (unit, errors) res =
  try
    Printf.printf "Attempting to write to file: %s\n" filepath;
    let columns = Hashtbl.fold (fun col _ acc -> col :: acc) table [] |> List.rev in
    Printf.printf "Columns: %s\n" (String.concat ", " columns);
   
    let data = List.map (fun col ->
      Printf.printf "Getting data for column: %s\n" col;
      Hashtbl.find table col
    ) columns in
   
    let rows = Data_manipulation.transpose data in
    Printf.printf "Number of rows: %d\n" (List.length rows);
   
    let csv_content =
      String.concat "," columns ::
      (List.map (fun row -> String.concat "," row) rows)
      |> String.concat "\n"
    in
   
    Printf.printf "About to write content:\n%s\n" csv_content;
    let oc = open_out filepath in
    output_string oc csv_content;
    close_out oc;
    Ok ()
  with
  | Not_found -> 
      Printf.printf "Not_found error occurred\n";
      Error (ParseCsvError "Inconsistent table structure")
  | Sys_error msg -> 
      Printf.printf "Sys_error: %s\n" msg;
      Error (ParseCsvError msg)
  | e -> 
      Printf.printf "Unknown error: %s\n" (Printexc.to_string e);
      Error (ParseCsvError "Failed to write CSV file")