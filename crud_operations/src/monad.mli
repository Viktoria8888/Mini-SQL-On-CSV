open Types

type ('a, 'e) t = ('a, 'e) res

val ( let* ) : ('a, errors) t -> ('a -> ('b, errors) t) -> ('b, errors) t
val return : 'a -> ('a, errors) t
(* val bind : ('a, errors) t -> ('a -> ('b, errors) t) -> ('b, errors) t *)

(* val handle : ('a, errors) t -> (errors -> 'a -> ('b, errors) t) -> ('b,
   errors) t *)
val run : ('a, errors) t -> 'a option
