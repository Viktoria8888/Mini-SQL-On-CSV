open Query_engine
open Types


let assert_error result expected_error =
  match result with
  | Error e -> assert (e = expected_error)
  | Ok _ -> failwith "Expected Error, got Ok"

let assert_ok result expected_value =
  match result with
  | Ok v -> assert (v = expected_value)
  | Error _ -> failwith "Expected Ok, got Error"


let mock_data : (string, column) Hashtbl.t = Hashtbl.create 100


let setup_mock_data () =
  Hashtbl.clear mock_data;
  Hashtbl.add mock_data "Age" ["30"; "40"; "25"];
  Hashtbl.add mock_data "Attrition" ["Yes"; "No"; "Yes"]

let csv_data : (string, column) Hashtbl.t = Hashtbl.create 200