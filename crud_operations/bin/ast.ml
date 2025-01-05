type condition = 
  | EQUAL
  (* | Greater *)
type command = 
  | CREATE of string * (string * string) list
  | READ of string * string * condition option
  | DELETE of string * condition option

(*SELECT * FROM plik.csv WHERE column-1 > 2*)