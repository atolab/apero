open Lwt.Infix

module Actor = struct 
  
  module type S = sig 
    type t
    type message

    val spawn : (t -> t -> message -> bool Lwt.t) -> t    
    val receive : (t -> message -> bool Lwt.t) -> t    
    val send : t ->  message -> t -> unit Lwt.t      
    val (>>) : t * message -> t ->  unit Lwt.t  
    val stop : t -> unit Lwt.t
    val run_loop : t -> unit Lwt.t
  end

  module Make (M : sig type message end) = struct 
    type message = M.message
    module EventStream = Event_stream.EventStream.Make(Stream_lwt.Stream)
    
    type sink = (t * message) EventStream.Sink.s
    and source = (t * message) EventStream.Source.s
    and t = 
      { self_sink : sink
      ; self_source : source 
      ; run_loop : unit Lwt.t
      ; completer : unit Lwt.u }
    
    
    let spawn handler  = 
      (* The size of the queue should be configurable *)
      let (self_source, self_sink) = EventStream.create 256 in       
      let (run_loop, completer) = Lwt.task () in 
      let self = { self_sink; self_source; run_loop; completer } in 
      let self_handler = handler self in 
      let rec loop _ = 
        match%lwt EventStream.Source.get self_source with 
        | Some (from,  msg) ->
          (self_handler from msg) >>= loop
        | None -> loop false
      in 
      Lwt.async (fun () -> loop true);  
      self

    let receive receiver  = 
      (* The size of the queue should be configurable *)
      let (self_source, self_sink) = EventStream.create 256 in      
      let (run_loop, completer) = Lwt.task () in 
      let self = { self_sink; self_source; run_loop; completer } in        
      let rec loop  = function
        | true -> (match%lwt EventStream.Source.get self_source with 
          | Some (from,  msg) -> (receiver from msg) >>= loop        
          | None -> loop true)
        | false -> Lwt.return_unit
      in  
        Lwt.async (fun () -> loop true) ;
        self
      

    let (>>) (self, message) target = EventStream.Sink.push (self, message) target.self_sink 
    
    let send  self message target = (self, message) >> target
    
    let stop _ = Lwt.return_unit
     
    let run_loop self = self.run_loop
    
  end

end 