
open Interp
open Data_manipulation

let mock_data : (string, column) Hashtbl.t = Hashtbl.create 100
let test_assign_cols () =
  let names = ["Age"; "Attrition"; "Performance"] in
  let cols = [["30"; "40"; "25"]; ["Yes"; "No"; "Yes"]; ["High"; "Low"; "Medium"]] in
  let () = assign_cols mock_data names cols in

  (* Check if the keys are present *)
  assert (Hashtbl.mem mock_data "Age");
  assert (Hashtbl.mem mock_data  "Attrition");
  assert (Hashtbl.mem mock_data "Performance");
  Printf.printf "test_assign_cols passed\n"

let test_transpose () =
  let input = [["a"; "b"; "c"]; ["d"; "e"; "f"]; ["g"; "h"; "i"]] in
  let expected = [["a"; "d"; "g"]; ["b"; "e"; "h"]; ["c"; "f"; "i"]] in
  let result = transpose input in
  assert (result = expected);
  Printf.printf "test_transpose passed\n"

let test_get_keys () =
  Hashtbl.add mock_data  "Age" ["30"; "40"; "25"];
  Hashtbl.add mock_data  "Attrition" ["Yes"; "No"; "Yes"];
  let keys = get_keys mock_data  in
  (* Check if "Age" and "Attrition" are in the keys *)
  assert (List.mem "Age" keys);
  assert (List.mem "Attrition" keys);
  Printf.printf "test_get_keys passed\n"
(* --testing on csv-- *)

let csv_data : (string, column) Hashtbl.t = Hashtbl.create 200
let test_read_csv_file () = 
  let filename = "../csv_files/username_file.csv" in
  let _ = read_csv_file filename csv_data in
  let keys = get_keys csv_data  in
  List.iter (fun x -> Printf.printf "Key: %s\n" x) keys;
  assert (List.mem "Last name" keys);
  assert (List.mem "First name" keys);
  assert (List.mem "Identifier" keys);
  assert (List.mem "Username" keys);
  (* match (Hashtbl.find_opt csv_data "Last name") with
    | (Some col) -> List.iter (fun x -> Printf.printf "%s\n" x) col;
    | None -> Printf.printf "not found\n" 
   *)
  Printf.printf "test_get_keys passed\n"
let () =
  (* Run tests *)
  (* Printf.printf "Current working directory: %s\n" (Sys.getcwd ()); *)

  test_assign_cols ();
  test_transpose ();
  test_get_keys ();
  test_read_csv_file ();