module Actor : sig 
  
  module type S = sig 
    type t
    type message

    val spawn : (t -> t -> message -> bool Lwt.t) -> t
    val receive : (t -> message -> bool Lwt.t) -> t    
    val send :  t -> message -> t -> unit Lwt.t     
    val (>>) :  t * message -> t -> unit Lwt.t      
    val stop : t -> unit Lwt.t
    val run_loop : t -> unit Lwt.t

  end

  module Make (M : sig type message end) : S with type message = M.message
end 