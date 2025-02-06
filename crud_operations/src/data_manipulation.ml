open Monad
open Types

let filename = "csv_files/username_file.csv"
let data : (string, column) Hashtbl.t = Hashtbl.create 100
let column_names : string list ref = ref []

let rec assign_cols table names cols =
  match (names, cols) with
  | [], [] -> ()
  | [], _ :: _ -> failwith "incorrect sizes"
  | _ :: _, [] -> failwith "incorrect sizes"
  | x :: xs, y :: ys ->
      Hashtbl.add table x y;
      assign_cols table xs ys
      (* Printf.printf "Adding column: %s: %b\n" x (Hashtbl.mem table x) *)

let rec transpose (xs : string list list) =
  match xs with
  | [] -> []
  | [] :: _ -> []
  | _ ->
      let fst_col = List.map List.hd xs in
      let rest = List.map List.tl xs in
      fst_col :: transpose rest

let new_form (xs : string list list) (table : (string, column) Hashtbl.t) : unit
    =
  let transposed = transpose xs in
  assign_cols table !column_names transposed
  (* Printf.printf "num of column names = num of columns after transposing: %b\n"
    (List.length !column_names = List.length transposed) *)
(* assert (List.length !column_names = List.length transposed+1) *)

(* List.iter (fun row -> Printf.printf "Row: %s\n" (String.concat ", " row))
   transposed *)

let read_csv_file filename table =
  try
    let file = Csv.load filename in
    match file with
    | [] -> Error EmptyFile
    | fst_row :: rows ->
        column_names := fst_row;
        new_form rows table;
        return data

  with
  | Sys_error msg ->
      Printf.printf "Error reading file: %s\n" msg;
      Error (FileNotFound filename)
  | Csv.Failure (_, _, msg) ->
      Printf.printf "CSV parsing error: %s\n" msg;
      Error (ParseCsvError filename)

let get_keys table = Hashtbl.fold (fun key _ acc -> key :: acc) table []


let check_table (name : string) : (string, errors) res =
let file_path = Filename.concat "../csv_files/" name in
if Sys.file_exists file_path then Ok file_path else Error (FileNotFound name)
