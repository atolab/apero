open Lwt.Infix

module ActorCoreMessage = struct 
  type core_message = [ `Timeout of float  | `Terminate | `EmptyMessage ]  [@@deriving show]
end 

module Actor : sig 
    

    type 'msg actor_mailbox
    type  'msg t  
    type ('msg, 's) reaction = ([> ActorCoreMessage.core_message ] as 'msg) t -> 's option -> 'msg actor_mailbox  option -> 'msg -> ('msg t * 's option * bool) Lwt.t 
    
    val addr :  ([> ActorCoreMessage.core_message ] as 'msg) t -> 'msg actor_mailbox

    val spawn : ?queue_len : int -> ?state : 's option -> ?timeout :  float option -> ('msg, 's) reaction -> ('msg actor_mailbox * unit Lwt.t)
    val set_timeout : ([> ActorCoreMessage.core_message ] as 'msg) t -> float option -> 'msg t
    val get_timeout : ([> ActorCoreMessage.core_message ] as 'msg) t -> float option
    
    val terminate : ([> ActorCoreMessage.core_message ] as 'msg) t -> 's option ->  bool ->  ('msg t * 's option * bool) Lwt.t
    val continue : ([> ActorCoreMessage.core_message ] as 'msg) t -> 's option -> bool ->  ('msg t * 's option * bool) Lwt.t
    
    val (<!>) :  ([> ActorCoreMessage.core_message ] as 'msg) actor_mailbox -> ('msg actor_mailbox option * 'msg) -> unit Lwt.t
    val send : ([> ActorCoreMessage.core_message ] as 'msg) actor_mailbox -> 'msg actor_mailbox option ->  'msg -> unit Lwt.t
         
    val maybe_send : ([> ActorCoreMessage.core_message ] as 'msg) actor_mailbox option -> 'msg actor_mailbox option ->  'msg -> bool Lwt.t
      
    val (<?!>) : ([> ActorCoreMessage.core_message ] as 'msg) actor_mailbox option -> ('msg actor_mailbox option *  'msg) -> bool Lwt.t
        
end = struct
    
    
    module EventStream = Event_stream.EventStream.Make(Stream_lwt.Stream)
    module ActorId = Id.Make(Int64)
    

    type 'msg actor_mailbox = ActorMailbox of 
      { inbox : ('msg actor_mailbox option * 'msg) EventStream.Source.s
      ; outbox:  ('msg actor_mailbox option * 'msg) EventStream.Sink.s }

    type  'msg t = 
      { mailbox : 'msg actor_mailbox  
      ; timeout : float option              
      ; run_loop : unit Lwt.t
      ; completer : unit Lwt.u 
      ; aid : ActorId.t} 
      
    type ('msg, 's) reaction = ([> ActorCoreMessage.core_message ] as 'msg) t -> 's option -> 'msg actor_mailbox  option -> 'msg -> ( 'msg t * 's option * bool) Lwt.t       

    let addr actor = actor.mailbox

    let spawn ?(queue_len=256) ?(state=None) ?(timeout=None) (handler : ('msg, 's) reaction) =                         
     
      let (inbox, outbox) = EventStream.create queue_len in
      let mailbox = ActorMailbox { inbox ; outbox } in 
      let (run_loop, completer) = Lwt.task () in     
      let self = 
        { mailbox
        ; timeout
        ; run_loop
        ; completer
        ; aid = ActorId.next_id () } in
        
      let rec loop (handler : ('msg, 's) reaction) (self, state, continue)  = 
        match continue with
        | true -> 
          (let ps = (EventStream.Source.get inbox >|= function  Some v ->  `ActorMessage v | None -> `EmptyMessage) :: 
          (match self.timeout with 
            | Some period ->  [ Lwt_unix.sleep period >|= fun () -> `Timeout period ]        
            | None -> []) in     

          match%lwt Lwt.pick ps with 
          | `ActorMessage (from,  msg) -> (handler self state from msg) >>= (loop handler)
          | `Timeout period  -> (handler self state None (`Timeout period)) >>= (loop handler)      
          | `EmptyMessage -> loop handler (self, state, false)
          | `Terminate -> (handler self state None `Terminate) >|=  fun _ -> (self, true))
        | false -> Lwt.return (self, false)
      in Lwt.async (fun () -> loop handler (self, state, true) )
      ; (self.mailbox, run_loop)
    
    let set_timeout a  timeout = {a with timeout}
    let get_timeout a  = a.timeout

    let terminate (a : ([> ActorCoreMessage.core_message ] as 'msg) t) (state: 's option)  (_ : bool) = Lwt.return (a, state, false)
    let continue (a : ([> ActorCoreMessage.core_message ] as 'msg) t) (state: 's option)  (_:bool) = Lwt.return (a, state, true)
    
        
    (* let receive ?(queue_len=256) ?(timeout=None) receiver  = 
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
          | `Timeout period  -> (receiver self None (`Timeout period)) >>= loop      
          | `EmptyMessage -> loop (self, true)
          | `Terminate -> (Lwt_io.printf "Stop!" >|= fun _ -> (self, true)))
        | false -> Lwt.return (self, false)
      in Lwt.async (fun () -> loop (self, true))
      ; self *)
    
    
    let send  (dest : ([> ActorCoreMessage.core_message ] as 'msg) actor_mailbox) (from : 'msg actor_mailbox option ) (message : 'msg) = match dest with 
      | ActorMailbox {inbox=_; outbox=outbox} -> EventStream.Sink.push  (from,message) outbox

    let (<!>) dest (from, message)  = send dest from message      
      
    let maybe_send (dest : ([> ActorCoreMessage.core_message ] as 'msg) actor_mailbox option) (from : 'msg actor_mailbox option) (message: 'msg) = match dest with       
      | Some actor -> (actor <!> (from, message)) >|= fun () -> true
      | None -> Lwt.return false
  
    let (<?!>) dest (from,message) = maybe_send dest from message
    

    (* let compare a b = ActorId.compare a.aid b.aid
    let (=) a b = ActorId.(equal) a.aid b.aid  *)

  end

  