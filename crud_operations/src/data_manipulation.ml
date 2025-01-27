open Monad
type column = string list

let filename = "csv_files/data.csv"
let data : (string, column) Hashtbl.t = Hashtbl.create 100
let column_names : string list ref = ref []

let rec assign_cols table names cols = 
  match (names, cols) with
  | ([], []) -> ()
  | ([], _::_) -> failwith "incorrect sizes"
  | (_::_, []) -> failwith "incorrect sizes"
  | (x :: xs, y :: ys) -> 
      Hashtbl.add table x y; 
      assign_cols table xs ys;
      Printf.printf "Adding column: %s: %b\n" x (Hashtbl.mem table x)

let rec transpose (xs: string list list) = 
  match xs with
  | []     -> []  
  | [] :: _ -> []
  | _      -> 
  let fst_col = List.map List.hd xs in
  let rest = List.map List.tl xs in
  fst_col :: transpose rest

let new_form (xs: string list list) (table: (string, column) Hashtbl.t): unit = 
  let transposed = transpose xs in
  assign_cols table !column_names transposed;
  Printf.printf "num of column names = num of columns after transposing: %b\n" (List.length !column_names = List.length transposed)
  (* assert (List.length !column_names = List.length transposed+1) *)

  (* List.iter (fun row -> Printf.printf "Row: %s\n" (String.concat ", " row)) transposed *)


let read_csv_file filename table =
  try
    let file = Csv.load filename in
    match file with
      | [] -> Error EmptyFile
      (* | fst_row::rows -> column_names := fst_row; new_form rows; ResultMonad.return data; *)
      | fst_row::rows -> column_names := fst_row; new_form rows table; ResultMonad.return data
    (* List.iter (fun row -> Printf.printf "%s\n" (String.concat ", " row)) data;
    data *)
  with
    | Sys_error msg -> Printf.printf "Error reading file: %s\n" msg;  Error FileNotFound
    | Csv.Failure (_,_, msg) -> Printf.printf "CSV parsing error: %s\n" msg; Error ParseCsvError 

let get_keys table =
  Hashtbl.fold (fun key _ acc -> key :: acc) table []
    
(* let () = 
  let _ = read_csv_file filename in
  Printf.printf "Does Age exist? %b\n" (Hashtbl.mem data "Age");; *)