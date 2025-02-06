(* open Interp open Eval open Monad

   let assert_error result expected_error = match result with | Error e ->
   assert (e = expected_error) | Ok _ -> failwith "Expected Error, got Ok"

   let assert_ok result expected_value = match result with | Ok v -> assert (v =
   expected_value) | Error _ -> failwith "Expected Ok, got Error"

   let test_check_table () = (* Test non-existent file *) assert_error
   (check_table "nonexistent.csv") (FileNotFound "nonexistent.csv");

   (* Test existing file *) let expected_path = "../csv_files/username.csv" in
   Printf.printf "%s\n" expected_path; let res = (check_table "username.csv") in
   (assert_ok res expected_path);

   Printf.printf "test_check_table passed!\n"

   let () = test_check_table () *)
