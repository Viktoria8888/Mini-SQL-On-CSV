let () =
  Test_data_manipulation.run_tests ();
  Printf.printf "\n";
  Test_parser.run_tests ();
  Printf.printf "\n";
  Test_query.run_tests ();