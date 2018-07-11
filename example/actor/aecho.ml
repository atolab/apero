open Apero

module Message = struct 
  type message = [`Msg of string | `MsgX of string * bool | `EchoX of string * bool |  `Echo of string]
end

module EchoActor = Actor.Make (Message)
open EchoActor
open Lwt.Infix


let () =  
  let echo_actor = spawn (fun self from -> function 
    | `Msg msg ->  ((self,`Echo msg) >> from) >|= (fun _ -> true)
    | `MsgX (msg, grow) -> ((self,`EchoX (msg, grow)) >> from) >|= (fun _ -> true)
    | _ -> Lwt.return_false) in 

  let periodic_actor = spawn (fun self from -> function 
    | `EchoX (msg, grow) -> 
      let _ = Lwt_io.printf  "Received: %s\n" msg in
      Lwt_unix.sleep 0.005 
      >>= fun () -> 
        (match String.length msg with 
        | 1 -> (self,`MsgX (msg ^ "A", true)) >> from         
        | 30 as len -> (self, `MsgX (String.sub msg 1 (len-1), false)) >> from
        | len ->  (match grow with 
          | true ->  (self,`MsgX (msg ^ "A", true)) >> from
          | false -> (self, `MsgX (String.sub msg 1 (len-1), false)) >> from))        
      >|= fun _ -> true
    | _ -> Lwt.return_false) in

  let client_actor = receive (fun _ -> function
    | `Echo msg -> let _ = Lwt_io.printf  "Received: %s\n" msg in Lwt.return_true
    | _ -> let _ = Lwt_io.printf "Unexpected message" in Lwt.return_false) in  
  
  let _ = (periodic_actor, `MsgX ("A", true)) >> echo_actor in
  let _ =  (client_actor, `Msg "B") >> echo_actor in

  Lwt_main.run @@ EchoActor.run_loop echo_actor
  
  