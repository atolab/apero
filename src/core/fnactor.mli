exception Terminate

val spawn :
  ?queue_len:int ->
  ?timeout:('b -> 'b) Actor.Actor.timeout_info option ->
  ?on_terminate:(unit -> 'b -> 'b) option ->
  'a -> ('a -> 'b) -> ('b -> 'c) -> 
  (('c -> 'a) -> unit) * unit Lwt.t

val terminate : 'a -> 'b

val pure : 'a -> 'b -> 'b * 'a
val readonly : ('a -> 'b) -> 'a -> 'a * 'b

val ( @%> ) : ('a -> 'b * 'c) -> ('c -> unit) -> 'a -> 'b
val ( %@> ) : ('a -> 'b * 'c) -> ('b -> unit) -> 'a -> 'c
