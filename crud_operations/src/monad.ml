open Types

type ('a, 'e) t = ('a, 'e) res

let return x = Ok x (* Return the value with the correct error type *)

let bind m f =
  match m with
  | Ok v -> f v
  | Error e -> Error e

let ( let* ) = bind

(*let join m = match m with | Ok (Ok x) -> Ok x | Ok (Error e) -> Error e |
  Error e -> Error e *)

let run m =
  match m with
  | Ok v -> Some v
  | Error _ -> None
