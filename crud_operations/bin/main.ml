open Query_engine
open Data_manipulation
open Types

let csv_data : (string, column) Hashtbl.t = Hashtbl.create 100

let () =
  if Array.length Sys.argv < 2 then
    Printf.printf "Usage: %s <filename>\n" Sys.argv.(0)
  else
    let filename = Sys.argv.(1) in
    let file_path = Filename.concat "csv_files/" filename in
    if Sys.file_exists file_path then (
      let _ = read_csv_file file_path csv_data in
      let keys = get_keys csv_data in
      List.iter (fun x -> Printf.printf "Key: %s\n" x) keys;
      Printf.printf "File found: %s\n" file_path)
    else Printf.printf "File not found: %s\n" file_path
