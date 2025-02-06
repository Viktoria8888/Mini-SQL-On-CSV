open Query_engine
open Types

val assert_error : ('a, errors) res -> errors -> unit
val assert_ok : ('a, errors) res -> 'a -> unit

val mock_data : (string, column) Hashtbl.t
val csv_data : (string, column) Hashtbl.t
val setup_mock_data : unit -> unit