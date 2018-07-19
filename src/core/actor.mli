module Actor : sig 
    
    type 'msg timeout_info = (float * (float -> 'msg))
    
    type ('mbox, 'msg) actor_message = 
      | ActorMessage of ('mbox option  * 'msg) 
      | Timeout of 'msg timeout_info  
      | Terminate 
      | EmptyMessage 

    type 'msg actor_mailbox
        
    type  'msg t  
    type ('msg, 's) reaction = 'msg t -> 's option -> 'msg actor_mailbox  option -> 'msg -> ('msg t * 's option * bool) Lwt.t 

    val addr :  'msg t -> 'msg actor_mailbox
    val spawn : ?queue_len:int -> ?state :'s option -> ?timeout:'msg timeout_info option -> ?on_terminate:(unit -> 'msg) option -> ('msg, 's) reaction -> ('msg actor_mailbox * unit Lwt.t)

    val set_timeout : 'msg t -> (float * (float -> 'msg)) option -> 'msg t
    val get_timeout : 'msg t -> (float * (float -> 'msg)) option

    val send : 'msg actor_mailbox -> 'msg actor_mailbox option ->  'msg -> unit Lwt.t
    val (<!>) : 'msg actor_mailbox -> ('msg actor_mailbox option * 'msg) -> unit Lwt.t

    val maybe_send : 'msg actor_mailbox option -> 'msg actor_mailbox option ->  'msg -> bool Lwt.t      
    val (<?!>) : 'msg actor_mailbox option -> ('msg actor_mailbox option *  'msg) -> bool Lwt.t 
    
    val close : 'msg actor_mailbox -> unit Lwt.t 
    
    val terminate : 'msg t -> 's option ->  bool ->  ('msg t * 's option * bool) Lwt.t
    val continue : 'msg t -> 's option -> bool ->  ('msg t * 's option * bool) Lwt.t
    

    val compare : 'msg actor_mailbox -> 'msg actor_mailbox -> int
    val (=) :  'msg actor_mailbox -> 'msg actor_mailbox -> bool
    
end 