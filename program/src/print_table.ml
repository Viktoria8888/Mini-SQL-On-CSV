let print_cols_data (columns: string list) (data: string list list) =
  (* Get maximum width for each column *)
  let get_widths () =
    List.map (fun col ->
      let col_width = String.length col in
      let data_width = List.fold_left (fun max_w row ->
        try 
          let val_width = String.length (List.nth row (List.length row - 1)) in
          max max_w val_width
        with _ -> max_w
      ) 0 data in
      max col_width data_width
    ) columns
  in

  let print_separator widths =
    Printf.printf "+";
    List.iter (fun w -> Printf.printf "%s+" (String.make w '-')) widths;
    Printf.printf "\n"
  in

  let print_row widths row =
    Printf.printf "|";
    List.iter2 (fun w v -> 
      Printf.printf " %-*s |" w v
    ) widths row;
    Printf.printf "\n"
  in

  let widths = get_widths () in
  
  (* Print header *)
  print_separator widths;
  print_row widths columns;
  print_separator widths;
  
  (* Re-organize data into rows *)
  let rec make_rows acc = function
    | [] | []::_ -> List.rev acc
    | rows -> 
        let row = List.map List.hd rows in
        let rest = List.map List.tl rows in
        make_rows (row::acc) rest
  in
  
  let rows = make_rows [] data in
  List.iter (print_row widths) rows;
  print_separator widths