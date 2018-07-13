open Lwt.Infix

module Actor = struct

    type 'a core_message = [`ActorMessage of 'a | `Timeout of float  | `Terminate | `EmptyMessage ]    
    module EventStream = Event_stream.EventStream.Make(Stream_lwt.Stream)
    module ActorId = Id.Make(Int64)
    
    type  'a t = 
      { self_sink : 'a EventStream.Sink.s
      ; self_source : 'a EventStream.Source.s
      ; timeout : float option              
      ; run_loop : unit Lwt.t
      ; completer : unit Lwt.u 
      ; aid : ActorId.t} 
      
    let spawn ?(queue_len=256) ?(state=None) ?(timeout=None) handler =       
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
          | `Timeout period  -> (handler self state None (`Timeout period)) >>= loop      
          | `EmptyMessage -> loop (self, state, false)
          | `Terminate -> (handler self state None `Terminate) >|=  fun _ -> (self, true))
        | false -> Lwt.return (self, false)
      in Lwt.async (fun () -> loop (self, state, true))
      ; self
    
    let set_timeout (a : 'a t)  timeout = {a with timeout}
    let get_timeout (a : 'a t)  = a.timeout

    let terminate (a : 'a t) (state : 's) _ = Lwt.return (a, state, false)
    let continue (a : 'a t) (state : 's) _ = Lwt.return (a, state, true)
    
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
          | `Timeout period  -> (receiver self None (`Timeout period)) >>= loop      
          | `EmptyMessage -> loop (self, true)
          | `Terminate -> (Lwt_io.printf "Stop!" >|= fun _ -> (self, true)))
        | false -> Lwt.return (self, false)
      in Lwt.async (fun () -> loop (self, true))
      ; self

      

    let (<!>) dest (from, message)  = EventStream.Sink.push (`ActorMessage (from,message)) dest.self_sink     
    let send  dest from message = dest <!> (from, message) 
         
    let maybe_send dest from message = match dest with 
      | Some actor -> actor <!> (from, message) >|= fun () -> true
      | None -> Lwt.return false
      
    let (<?>) dest (from,message) = maybe_send dest from message
    
    let run_loop self = self.run_loop

    let compare a b = ActorId.compare a.aid b.aid
    let (=) a b = ActorId.(equal) a.aid b.aid 

  end