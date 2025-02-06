(* test_query.ml *)
open Query_engine
open Ast
open Query
open Print_table
open Test_utils

let setup_test_data table =
  let names = ["Age"; "Salary"; "Department"] in
  let cols = [
    ["25"; "30"; "35"; "40"];                   (* Age *)
    ["50000"; "60000"; "75000"; "80000"];       (* Salary *)
    ["IT"; "HR"; "Sales"; "IT"]                 (* Department *)
  ] in
  Data_manipulation.assign_cols table names cols

let test_eval_read () =
  let table = Hashtbl.create 10 in
  setup_test_data table;
  
  (* Test read without condition *)
  let cmd = Read("Age", "", None) in
  let result = eval cmd table in
  (match result with
  | Ok (Left res) -> 
      assert (res.columns = ["Age"]);
      assert (List.length (List.hd res.data) = 4);
      assert (List.hd res.data = ["25"; "30"; "35"; "40"])
  | _ -> failwith "Expected Ok (Left result)");

  (* Test read with Equal condition *)
  let cond = Equal("Age", "30") in
  let cmd = Read("Salary", "", Some cond) in
  let result = eval cmd table in
  (match result with
  | Ok (Left res) -> 
      assert (res.columns = ["Salary"]);
      assert (List.hd res.data = ["60000"])
  | _ -> failwith "Expected Ok (Left result)");

  Printf.printf "test_eval_read passed!\n"

let test_eval_select_all () =
  let table = Hashtbl.create 10 in
  setup_test_data table;

  (* Test select all without condition *)
  let cmd = SelectAll("", None) in
  let result = eval cmd table in
  (match result with
  | Ok (Left res) -> 
      assert (List.length res.columns = 3);
      print_cols_data res.columns res.data;
      assert (List.length res.data = 3)
  | _ -> failwith "Expected Ok (Left result)");

  Printf.printf "!Test select all with Greater condition!\n";
  let cond = Greater("Salary", "70000") in
  let cmd = SelectAll("", Some cond) in
  let result = eval cmd table in
  (match result with
  | Ok (Left res) -> (
      print_cols_data res.columns res.data;
      assert (List.length res.columns = 3);
      assert (List.length (List.hd res.data) = 2)) (* Should only get rows where salary > 70000 *)
  | _ -> failwith "Expected Ok (Left result)");

  Printf.printf "test_eval_select_all passed!\n"

let test_eval_delete () =
  let table = Hashtbl.create 10 in
  setup_test_data table;
    
  (* Test delete rows with condition *)
  let cond = Greater("Salary", "60000") in
  let cmd = Delete("test_delete.csv", Some cond) in  (* Include filename in command *)
  let result = eval cmd table in
  (match result with
  | Ok (Right msg) ->
      assert (msg = "Rows matching condition were deleted");
      let salary_col = Hashtbl.find table "Salary" in
      assert (not (List.mem "75000" salary_col));
      assert (not (List.mem "80000" salary_col))
  | Ok (Left _) -> failwith "Expected Right result, got Left"
  | Error InvalidCondition -> failwith "Invalid condition"
  | Error (FileNotFound f) -> failwith ("File not found: " ^ f)
  | Error _ -> failwith "Unexpected error11");
  
  Printf.printf "test_eval_delete passed!\n"
let test_error_cases () =
  let table = Hashtbl.create 10 in
  setup_test_data table;

  (* Test column not found *)
  let cmd = Read("NonExistent", "", None) in
  let result = eval cmd table in
  assert_error result (ColumnNotFound "NonExistent");

  (* Test invalid condition (non-integer comparison) *)
  let cond = Equal("Department", "5") in
  let cmd = Read("Age", "", Some cond) in
  let result = eval cmd table in
  assert_error result InvalidCondition;

  Printf.printf "test_error_cases passed!\n"

let run_tests () =
  Printf.printf "---Testing query---\n";
  test_eval_read ();
  test_eval_select_all ();
  test_eval_delete ();
  test_error_cases ()