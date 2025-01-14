type condition = 
  | Equal of   string  * string
  | Greater of string  * string
  
type command = (*starting rule for parsing*)
  (* | Create of string * (string * string) list *)
  | Read of string * string * condition option
  | Delete of string * condition option
  | SelectAll of string * condition option

(*SELECT * FROM plik.csv WHERE column-1 > 2*)