
type ('a, 'e) res =
  | Ok of 'a
  | Error of 'e

type errors = 
  | FileNotFound
  | PermissionDenied
  | EmptyFile
  | ParseCsvError
module ResultMonad : sig
  type ('a, 'e) t = ('a, 'e) res
  val return : 'a -> ('a, errors) t  
  val bind : ('a, errors) t -> ('a -> ('b, errors) t) -> ('b, errors) t
  (* val handle : ('a, errors) t -> (errors -> 'a -> ('b, errors) t) -> ('b, errors) t *)
  val run : ('a, errors) t -> 'a option
end