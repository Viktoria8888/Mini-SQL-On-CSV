(* test_update_csv.ml *)
open Query_engine
open Types
open Update_csv

let test_write_csv () =
  (* Create directory in current directory *)
  let csv_dir = "./test_csv_files" in
  if not (Sys.file_exists csv_dir) then
    Sys.mkdir csv_dir 0o755;
  
  let table = Hashtbl.create 10 in
  Hashtbl.add table "Name" ["Alice"; "Bob"];
  Hashtbl.add table "Age" ["25"; "30"];
  
  let test_file = "test_output.csv" in
  
  (* Update write_csv_file function to use the test directory *)
  match write_csv_file (Filename.concat csv_dir test_file) table with
  | Ok () -> 
      assert (Sys.file_exists (Filename.concat csv_dir test_file));
      (* Verify file content *)
      let content = In_channel.with_open_text (Filename.concat csv_dir test_file) In_channel.input_all in
      Printf.printf "File content:\n%s\n" content;
      Printf.printf "test_write_csv passed!\n"
  | Error (ParseCsvError msg) -> 
      failwith ("Failed to write CSV: " ^ msg)
  | Error _ -> 
      failwith "Unknown error occurred"

let run_tests () =
  Printf.printf "---Testing CSV writing---\n";
  test_write_csv ()