open Query_engine
open Data_manipulation
open Test_utils

let test_assign_cols () =
  let names = ["Age"; "Attrition"; "Performance"] in
  let cols = [
    ["30"; "40"; "25"];
    ["Yes"; "No"; "Yes"];
    ["High"; "Low"; "Medium"]
  ] in
  let () = assign_cols mock_data names cols in
  assert (Hashtbl.mem mock_data "Age");
  assert (Hashtbl.mem mock_data "Attrition");
  assert (Hashtbl.mem mock_data "Performance");
  Printf.printf "test_assign_cols passed\n"

let test_transpose () =
  let input = [["a"; "b"; "c"]; ["d"; "e"; "f"]; ["g"; "h"; "i"]] in
  let expected = [["a"; "d"; "g"]; ["b"; "e"; "h"]; ["c"; "f"; "i"]] in
  let result = transpose input in
  assert (result = expected);
  Printf.printf "test_transpose passed\n"

let test_get_keys () =
  Hashtbl.add mock_data "Age" ["30"; "40"; "25"];
  Hashtbl.add mock_data "Attrition" ["Yes"; "No"; "Yes"];
  let keys = get_keys mock_data in
  assert (List.mem "Age" keys);
  assert (List.mem "Attrition" keys);
  Printf.printf "test_get_keys passed\n"

let test_read_csv_file () =
  let filename = "../csv_files/username_file.csv" in
  let _ = read_csv_file filename csv_data in
  let keys = get_keys csv_data in
  List.iter (fun x -> Printf.printf "Key: %s\n" x) keys;
  assert (List.mem "Last name" keys);
  assert (List.mem "First name" keys);
  assert (List.mem "Identifier" keys);
  assert (List.mem "Username" keys);
  Printf.printf "test_get_keys passed\n"

let test_check_table () =
  (* Test non-existent file *)
  assert_error (check_table "nonexistent.csv") (FileNotFound "nonexistent.csv");

  (* Test existing file *)
  let expected_path = "../csv_files/username.csv" in
  Printf.printf "%s\n" expected_path;
  let res = check_table "username.csv" in
  assert_ok res expected_path;
  Printf.printf "test_check_table passed!\n"
let run_tests () =
  Printf.printf "---Testing data manipulation---\n";
  test_assign_cols ();
  test_transpose ();
  test_get_keys ();
  test_read_csv_file ();
  test_check_table ()