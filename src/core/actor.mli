module Actor : sig 
  type core_message = [`Timeout of float  | `Terminate]

  module type S = sig 
    type t
    
    type message    

    val spawn : ?queue_len:int -> ?state:'s option -> ?timeout:float option -> (t -> 's option -> t option -> message -> (t * 's option * bool) Lwt.t) -> t
    val receive : ?queue_len:int ->  ?timeout:float option -> (t -> t option -> message -> (t * bool) Lwt.t) -> t    
    val send :  t -> t option -> message  -> unit Lwt.t     
    val (<!>) :  t -> t option * message -> unit Lwt.t   
    val maybe_send :  t option -> t option -> message  ->  bool Lwt.t     
    val (<?>) :  t option -> t option * message ->  bool Lwt.t          
    val run_loop : t -> unit Lwt.t    
    val set_timeout : t -> float option -> t
    val get_timeout : t -> float option  
    val terminate : t -> 's option -> bool -> (t * 's option * bool) Lwt.t
    val continue : t -> 's option -> bool ->(t * 's option * bool) Lwt.t
    val compare : t -> t -> int
    val (=) : t -> t -> bool    
  end

  module Make (M : sig  type message  
                        val timeout : float -> message 
                   end) : S with type message = M.message

end 