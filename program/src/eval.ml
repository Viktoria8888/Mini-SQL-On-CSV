(* open Types open Ast open Monad open Data_manipulation type table_result = {
   columns: string list; data: string list list; }

   let parse (s : string) : (command, errors) res = try let lexbuf =
   Lexing.from_string s in let ast = Parser.main Lexer.tokenize lexbuf in Ok ast
   with | _ -> Error (ParseError "failed to parse query")

   let check_table (name : string) : (string, errors) res = let file_path =
   Filename.concat "../csv_files/" name in if Sys.file_exists file_path then Ok
   file_path else Error (FileNotFound name)

   let filter_on_cond vals vals_for_cond f = List.filteri (fun i _ -> f
   (List.nth vals_for_cond i)) vals

   let eval_read (cond_opt : condition option) (col: string) (values : string
   list) (table : (string, column) Hashtbl.t) : (table_result, errors) res = let
   result = match cond_opt with | Some cond -> ( match cond with | Equal (name,
   v) -> ( match Hashtbl.find_opt table name with | Some vals_for_cond -> ( try
   let v = int_of_string v in Ok (filter_on_cond values vals_for_cond (fun x ->
   int_of_string x = v)) with | _ -> Error InvalidCondition) | None -> Error
   (ColumnNotFound name)) | Greater (name, v) -> ( match Hashtbl.find_opt table
   name with | Some vals_for_cond -> ( try let v = int_of_string v in Ok
   (filter_on_cond values vals_for_cond (fun x -> int_of_string x > v)) with | _
   -> Error InvalidCondition) | None -> Error (ColumnNotFound name))) | None ->
   Ok values in match result with | Ok filtered_data -> Ok { columns = [col];
   data = [filtered_data] } | Error e -> Error e

   let get_all_values (table : (string, column) Hashtbl.t) : string list list =
   Hashtbl.fold (fun _ values acc -> values :: acc) table []

   let eval_select_all (cond_opt : condition option) (table : (string, column)
   Hashtbl.t) : (table_result, errors) res = let columns = Hashtbl.fold (fun col
   _ acc -> col :: acc) table [] in let values = get_all_values table in let
   result = match cond_opt with | Some cond -> ( match cond with | Equal (name,
   v) -> ( match Hashtbl.find_opt table name with | Some vals_for_cond -> ( try
   let v = int_of_string v in Ok (filter_on_cond values vals_for_cond (fun x ->
   int_of_string x = v)) with | _ -> Error InvalidCondition) | None -> Error
   (ColumnNotFound name)) | Greater (name, v) -> ( match Hashtbl.find_opt table
   name with | Some vals_for_cond -> ( try let v = int_of_string v in Ok
   (filter_on_cond values vals_for_cond (fun x -> int_of_string x > v)) with | _
   -> Error InvalidCondition) | None -> Error (ColumnNotFound name))) | None ->
   Ok values in match result with | Ok filtered_data -> Ok { columns; data =
   filtered_data } | Error e -> Error e

   let eval_del (table: (string, column) Hashtbl.t) (cond_opt: condition
   option): (string, errors) res = match cond_opt with | Some cond -> () | None
   -> Hashtbl.remove table

   let values = get_all_values table in let rows = transpose values in
   List.filteri (fun i x -> )

   type query_res = ((table_result, errors) res, (string, errors) res) either

   let eval (q : command) (table : (string, column) Hashtbl.t) : query_res = let
   result = match q with | Read (col, _, cond_opt) -> ( match Hashtbl.find_opt
   table col with | Some values -> (eval_read cond_opt col values table) | None
   -> Error (ColumnNotFound col))

   | SelectAll (_, cond_opt) -> eval_select_all cond_opt table

   | Delete (col, cont_opt) -> ( match Hashtbl.find_opt table col with | Some _
   -> eval_del cond_opt col values table | None -> Error (ColumnNotFound col))

   | None -> Ok values in match result with | Ok filtered_data -> Ok { columns;
   data = filtered_data } | Error e -> Error e *)
