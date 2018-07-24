exception Terminate

val spawn :
  ?queue_len:int ->
  ?timeout:('t -> 't) Actor.Actor.timeout_info option ->
  ?on_terminate: (unit -> ('t -> 't)) option ->
  't -> ('t -> 'u) -> ('u -> 't) -> 
  (('t -> 't) -> unit Lwt.t) * unit Lwt.t

val terminate : 't -> 't

val pure : 'a -> 't -> 't * 'a
val readonly : ('t -> 'a) -> 't -> 't * 'a

val ( @%> ) : ('a -> 'b * 'c) -> ('c -> 'd) -> 'a -> 'b
val ( %@> ) : ('a -> 'b * 'c) -> ('b -> 'd) -> 'a -> 'c
