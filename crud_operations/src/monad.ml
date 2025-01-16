
type ('a, 'e) res =
  | Ok of 'a
  | Error of 'e

type errors = 
  | FileNotFound
  | PermissionDenied
  | EmptyFile
  | ParseCsvError
module ResultMonad = struct
  

  type ('a, 'e) t = ('a, 'e) res

  let return x = Ok x  (* Return the value with the correct error type *)

  let bind m f =
    match m with
    | Ok v -> f v
    | Error e -> Error e

  (* let handle m f =
    match m with
    | Ok v -> Ok v
    | Error e -> f e *)

  let run m =
    match m with
    | Ok v -> Some v
    | Error _ -> None
end