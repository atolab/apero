exception Terminate

type 't task = 't -> 't Lwt.t

val spawn :
  ?queue_len:int ->
  ?timeout:('t task) Actor.Actor.timeout_info option ->
  ?on_terminate: (unit -> ('t task)) option ->
  't -> ('t -> 'u) -> ('u -> 't) -> 
  (('t task) -> unit Lwt.t) * unit Lwt.t 
  
val terminate : 't task

val pure : 'a -> 't -> ('a * 't) Lwt.t
val readonly : ('t -> 'a Lwt.t) -> 't -> ('a * 't) Lwt.t

val ( @%> ) : ('a -> 'b * 'c) -> ('c -> 'd) -> 'a -> 'b
val ( %@> ) : ('a -> 'b * 'c) -> ('b -> 'd) -> 'a -> 'c

val ( @%>> ) : ('a -> ('b * 'c)Lwt.t) -> ('c Lwt.t -> 'd) -> 'a -> 'b Lwt.t
val ( %@>> ) : ('a -> ('b * 'c)Lwt.t) -> ('b Lwt.t -> 'd) -> 'a -> 'c Lwt.t

val ( @%>>= ) : ('a -> ('b * 'c)Lwt.t) -> ('c -> 'd) -> 'a -> 'b Lwt.t
val ( %@>>= ) : ('a -> ('b * 'c)Lwt.t) -> ('b -> 'd) -> 'a -> 'c Lwt.t
