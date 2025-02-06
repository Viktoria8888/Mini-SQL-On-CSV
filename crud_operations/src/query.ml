open Types
open Monad
open Ast

let filter_on_cond (vals: string list)  (vals_for_cond: string list) f =
  List.filteri (fun i _ -> f (List.nth vals_for_cond i)) vals

let eval_read (cond_opt : condition option) (col : string)
    (values : string list) (table : (string, column) Hashtbl.t) :
    (table_result, errors) res =
  let result =
    match cond_opt with
    | Some cond -> (
        match cond with
        | Equal (name, v) -> (
            match Hashtbl.find_opt table name with
            | Some vals_for_cond -> (
                try
                  let v = int_of_string v in
                  return
                    (filter_on_cond values vals_for_cond (fun x ->
                         int_of_string x = v))
                with
                | _ -> Error InvalidCondition)
            | None -> Error (ColumnNotFound name))
        | Greater (name, v) -> (
            match Hashtbl.find_opt table name with
            | Some vals_for_cond -> (
                try
                  let v = int_of_string v in
                  return
                    (filter_on_cond values vals_for_cond (fun x ->
                         int_of_string x > v))
                with
                | _ -> Error InvalidCondition)
            | None -> Error (ColumnNotFound name)))
    | None -> return values
  in
  match result with
  | Ok filtered_data -> return { columns = [ col ]; data = [ filtered_data ] }
  | Error e -> Error e

let get_all_values (table : (string, column) Hashtbl.t) : string list list =
  Hashtbl.fold (fun _ values acc -> values :: acc) table []

let eval_select_all (cond_opt : condition option)
    (table : (string, column) Hashtbl.t) : (table_result, errors) res =
  let columns = Hashtbl.fold (fun col _ acc -> col :: acc) table [] in
  let values = get_all_values table in
  let result =
    match cond_opt with
    | Some cond -> (
        match cond with
        | Equal (name, v) -> (
            match Hashtbl.find_opt table name with
            | Some vals_for_cond -> (
                try
                  let v = int_of_string v in
                  let len = List.length vals_for_cond in
                  let to_keep = Array.make len false in
                  List.iteri
                    (fun i x -> if int_of_string x = v then to_keep.(i) <- true)
                    vals_for_cond;
                  
                  return (List.map 
                    (fun col -> List.filteri (fun i _ -> to_keep.(i)) col) 
                    values)
                    
                with
                | _ -> Error InvalidCondition)
            | None -> Error (ColumnNotFound name))
        | Greater (name, v) -> (
            match Hashtbl.find_opt table name with
            | Some vals_for_cond -> (
                try
                  let v = int_of_string v in
                  let len = List.length vals_for_cond in
                  let to_keep = Array.make len false in
                  List.iteri
                    (fun i x -> if int_of_string x > v then to_keep.(i) <- true)
                    vals_for_cond;
                  
                  return (List.map 
                    (fun col -> List.filteri (fun i _ -> to_keep.(i)) col) 
                    values)
                    
                with
                | _ -> Error InvalidCondition)
            | None -> Error (ColumnNotFound name)))
    | None -> return values
  in
  match result with
  | Ok filtered_data -> return { columns; data = filtered_data }
  | Error e -> Error e

let eval_del (cond_opt : condition option) 
    (table : (string, column) Hashtbl.t) : (string, errors) res =
  match cond_opt with
  | Some cond -> (
      match cond with
      | Equal (name, v) -> (
          match Hashtbl.find_opt table name with
          | Some vals_for_cond -> (
              try
                let v = int_of_string v in
                let len = List.length vals_for_cond in
                let to_delete = Array.make len false in

                (* Marking indexes to delete - O(n) *)
                List.iteri
                  (fun i x -> if int_of_string x = v then to_delete.(i) <- true)
                  vals_for_cond;

                (* Update each column - now O(n) *)
                Hashtbl.iter
                  (fun col_name values ->
                    let new_values =
                      List.filteri (fun i _ -> not to_delete.(i)) values
                    in
                    Hashtbl.replace table col_name new_values)
                  table;

                return "Rows matching condition were deleted"
              with
              | _ -> Error InvalidCondition)
          | None -> Error (ColumnNotFound name))
      | Greater (name, v) -> (
          match Hashtbl.find_opt table name with
          | Some vals_for_cond -> (
              try
                let v = int_of_string v in
                let len = List.length vals_for_cond in
                let to_delete = Array.make len false in

                List.iteri
                  (fun i x -> if int_of_string x > v then to_delete.(i) <- true)
                  vals_for_cond;

                Hashtbl.iter
                  (fun col_name values ->
                    let new_values =
                      List.filteri (fun i _ -> not to_delete.(i)) values
                    in
                    Hashtbl.replace table col_name new_values)
                  table;

                return "Rows matching condition were deleted"
              with
              | _ -> Error InvalidCondition)
          | None -> Error (ColumnNotFound name)))
  | None -> Error (InvalidCondition)

let eval (q : command) (table : (string, column) Hashtbl.t) : query_res =
  match q with
  | Read (col, _, cond_opt) -> (
      match Hashtbl.find_opt table col with
      | Some (values : string list) ->
          let* result = eval_read cond_opt col values table in
          return (Left result)
      | None -> Error (ColumnNotFound col))
  | SelectAll (_, cond_opt) ->
      let* result = eval_select_all cond_opt table in
      return (Left result)
  | Delete (tab_name, cond_opt) -> (
    let* msg = (eval_del cond_opt table) in
    let full_path = Filename.concat "../csv_files/" tab_name in
    (match (Update_csv.write_csv_file full_path table) with
    | Ok () -> return (Right msg)
    | Error e -> Error e)
  )
