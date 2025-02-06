(* types.ml *)
type ('a, 'b) either =
  | Left of 'a
  | Right of 'b

type errors =
  | FileNotFound of string
  | ParseError of string
  | ColumnNotFound of string
  | ParseCsvError of string
  | InvalidCondition
  | EmptyFile

type column = string list

type ('a, 'e) res =
  | Ok of 'a
  | Error of 'e

type table_result = {
    columns : string list
  ; data : string list list
}

type query_res = ((table_result, string) either, errors) res
