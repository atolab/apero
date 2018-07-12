open Apero
open Lwt.Infix


  
module Message = struct 
  type message = [`Msg of string | `MsgX of string * bool | `EchoX of string * bool |  `Echo of string | `GetCount | `Count of int | `Timeout of float | Actor.core_message]   
  let timeout p = `Timeout p
end

module EchoActor = Actor.Make (Message)
open EchoActor


module EchoActorRegistry = struct 
  module EchoActorMap = Map.Make(String)
  type  t = { map :  (EchoActor.t EchoActorMap.t) Lwt_mvar.t }

  let self = { map = Lwt_mvar.create EchoActorMap.empty  }

  let register name a  =     
    Lwt_mvar.take self.map 
    >|=  EchoActorMap.add name a 
    >>= Lwt_mvar.put self.map
  
  let lookup name =     
    Lwt_mvar.take self.map 
    >|= fun map -> 
      let _ = Lwt_mvar.put self.map map in
      EchoActorMap.find_opt name map
          
end


let (<!>) = EchoActor.(<!>)
let (<?>) = EchoActor.(<?>)

let inc_count = function 
| Some v -> Some (v + 1)
| None -> None

let safe_get = function
| Some v -> v 
| None -> 0

let apppend_state sxs s = 
  match sxs with 
  | Some xs -> Some  (s::xs)
  | None -> Some [s]

let () =  
  Printexc.record_backtrace true ; 
  let echo_stateful_actor = spawn ~state:(Some 0) (fun self state from -> function 
    | `Msg msg ->          
        let state = inc_count state in 
        from <?> (Some self,`Echo msg) >>= continue self state
    | `MsgX (msg, grow) ->         
        let state = inc_count state in 
        from <?> (Some self,`EchoX (msg, grow)) >>= continue self state
    | `GetCount ->         
        from <?> (Some self, `Count (safe_get state)) >>= continue self state        
    | _ -> 
      Lwt.ignore_result @@ Lwt_io.printf "--- > echo_stateful_actor: Received unexpexted message -- TERMINATING < ---- \n" 
      ; (Lwt.return_false >>= terminate self state)) in
  
  let echo_actor = spawn (fun self state from -> function 
    | `Msg msg ->  from <?> (Some self,`Echo msg) >>= continue self state 
    | `MsgX (msg, grow) -> from <?> (Some self,`EchoX (msg, grow)) >>= continue self state 
    | _ -> 
      Lwt.ignore_result @@ Lwt_io.printf "--- > echo_actor: Received unexpexted message -- SKIPPING < ---- \n" 
      ; Lwt.return_true >>= continue self state ) in 

  let periodic_actor = spawn ~timeout:(Some 0.1) (fun self state from -> function 
    | `EchoX (msg, grow) -> 
      let _ = Lwt_io.printf  "Periodic Actor Received: %s\n" msg in
      let c =  String.get msg 0 in 

      let state = apppend_state state (match String.length msg with 
      | 1 ->  (Printf.sprintf "%s%c" msg c, true, from) 
        
      | 10 as len -> 
        let _ = from <?> (Some self, `GetCount) in
        (String.sub msg 1 (len-1), false, from)         

      | len ->  (match grow with 
        | true ->  (Printf.sprintf "%s%c" msg c, true, from)
          
        | false ->  (String.sub msg 1 (len-1), false, from)))
        in continue self state true
      
    | `Count n -> Lwt.ignore_result @@ Lwt_io.printf ">>> Echo count : %d\n" n 
      ; continue self state true
    
    | `Timeout _ -> 
      (match state with 
      | Some xs -> 
        xs |> List.iter (fun (msg, grow, from) -> let _ = from <?> (Some self, `MsgX (msg, grow)) in ()) 
        ; continue self (Some []) true

      | None -> continue self (Some []) true )
    | `Terminate -> terminate self state false
    | _ -> 
      Lwt.ignore_result @@ Lwt_io.printf "--- > periodic_actor: Received unexpexted message -- SKIPPING < ---- \n" 
      ; Lwt.return_true >>= continue self state) in 

  
  let client_actor = receive (fun self _ -> function
    | `Echo msg -> let _ = Lwt_io.printf  "Received: %s\n" msg in Lwt.return (self, true)
    | _ -> let _ = Lwt_io.printf "Unexpected message" in Lwt.return (self, false)) in  
  
  let _ = echo_actor <!> (Some periodic_actor, `MsgX ("A", true)) in
  let _ = echo_stateful_actor <!> (Some periodic_actor, `MsgX ("S", true)) in
  let _ = echo_actor <!> (Some client_actor, `Msg "B")  in

  let _ = EchoActorRegistry.register "echoactor.periodic.query" periodic_actor
  and _ = EchoActorRegistry.register "echoactor.stateful" echo_stateful_actor 
  and _ = EchoActorRegistry.register "echoactor.stateles" echo_actor in 
   Lwt_main.run @@ run_loop echo_actor
  