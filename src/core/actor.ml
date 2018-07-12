open Lwt.Infix


module Actor = struct 
  type core_message = [`Timeout of float | `EmptyMessage | `Terminate]
  
  module type S = sig 
    type t
    
    type message    
    
    val spawn : ?queue_len:int -> ?state:'s option -> ?timeout:float option -> (t -> 's option -> t option -> message -> (t * 's option * bool) Lwt.t) -> t
    val receive : ?queue_len:int  -> ?timeout:float option -> (t -> t option -> message -> (t * bool) Lwt.t) -> t    
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
                   end) = struct   
    type message = M.message
    type 'a actor_message = [ `ActorMessage of ('a option * message) | core_message]    

    module EventStream = Event_stream.EventStream.Make(Stream_lwt.Stream)
    module ActorId = Id.Make(Int64)

    type  sink =  (t actor_message) EventStream.Sink.s
    and   source = (t actor_message) EventStream.Source.s
    and t = 
      { self_sink : sink
      ; self_source : source 
      ; timeout : float option              
      ; run_loop : unit Lwt.t
      ; completer : unit Lwt.u 
      ; aid : ActorId.t}
      
    let spawn ?(queue_len=256) ?(state=None) ?(timeout=None) (handler:(t -> 's option -> t option -> message -> (t * 's option * bool) Lwt.t)) =       
      let (self_source, self_sink) = EventStream.create queue_len in       
      let (run_loop, completer) = Lwt.task () in     
      let self = { 
          self_sink
        ; self_source
        ; run_loop
        ; completer
        ; timeout 
        ; aid = ActorId.next_id () } in    
      let rec loop (self, state, continue) = 
        match continue with
        | true -> 
          (let ps = (EventStream.Source.get self_source >|= function  Some v ->  v | None -> `EmptyMessage) :: 
          (match self.timeout with 
            | Some period ->  [ Lwt_unix.sleep period >|= fun () -> `Timeout period ]        
            | None -> []) in     

          match%lwt Lwt.pick ps with 
          | `ActorMessage (from,  msg) -> (handler self state from msg) >>= loop
          | `Timeout period  -> (handler self state None (M.timeout period)) >>= loop      
          | `EmptyMessage -> loop (self, state, false)
          | `Terminate -> (Lwt_io.printf "Stop!" >|= fun _ -> (self, true)))
        | false -> Lwt.return (self, false)
      in Lwt.async (fun () -> loop (self, state, true))
      ; self
    
    let set_timeout (a : t)  timeout = {a with timeout}
    let get_timeout (a : t)  = a.timeout

    let terminate (a : t) (state : 's) _ = Lwt.return (a, state, false)
    let continue (a : t) (state : 's) _ = Lwt.return (a, state, true)
    
    let receive ?(queue_len=256) ?(timeout=None) receiver  = 
       let (self_source, self_sink) = EventStream.create queue_len in       
      let (run_loop, completer) = Lwt.task () in     
      let self = { 
          self_sink
        ; self_source
        ; run_loop
        ; completer
        ; timeout 
        ; aid = ActorId.next_id ()} in    
      let rec loop (self, continue) = 
        match continue with 
        | true -> 
          let ps = (EventStream.Source.get self_source >|= function  Some v ->  v | None -> `EmptyMessage) :: 
          (match self.timeout with 
            | Some period ->  [ Lwt_unix.sleep period >|= fun () -> `Timeout period ]        
            | None -> []) in     
          (match%lwt Lwt.pick ps with 
          | `ActorMessage (from,  msg) -> (receiver self from msg) >>= loop
          | `Timeout period  -> (receiver self None (M.timeout period)) >>= loop      
          | `EmptyMessage -> loop (self, true)
          | `Terminate -> (Lwt_io.printf "Stop!" >|= fun _ -> (self, true)))
        | false -> Lwt.return (self, false)
      in Lwt.async (fun () -> loop (self, true))
      ; self

      

    let (<!>) dest (from, message)  = EventStream.Sink.push (`ActorMessage (from,message)) dest.self_sink     
    let send  dest from message = dest <!> (from, message) 
         
    let maybe_send (dest: t option) (from: t option) (message: message) = match dest with 
      | Some actor -> actor <!> (from, message) >|= fun () -> true
      | None -> Lwt.return false
      
    let (<?>) dest (from,message) = maybe_send dest from message
    
    let run_loop self = self.run_loop

    let compare a b = ActorId.compare a.aid b.aid
    let (=) a b = ActorId.(equal) a.aid b.aid 

  end

end 