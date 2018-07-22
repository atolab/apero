open Apero
open Lwt.Infix

module EchoMessage = struct 
  type message = [`Msg of string | `Baz | `MsgX of string * bool | `EchoX of string * bool |  `Echo of string | `GetCount | `Count of int | `Timeout of float | `Terminate] [@@deriving show]
end


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

  
let test_two () = 
    let open Actor in 
    
    let  (stateful_actor, s_loop) = spawn  ~state:(Some 0) (fun self state from -> function 
        | `Msg msg as pmsg->          
            Lwt.ignore_result @@ Lwt_io.printf ">> Stateful Actor received: %s\n" @@ EchoMessage.show_message pmsg;
            let myaddr = addr self in
            let state = inc_count state in 
            let emsg : EchoMessage.message = `Echo msg in 
            from <?!> (Some myaddr,emsg) >>= continue self state
        | `MsgX (msg, grow) as pmsg->         
            Lwt.ignore_result @@ Lwt_io.printf ">> Stateful Actor received: %s\n" @@ EchoMessage.show_message pmsg;
            let myaddr = addr self in
            let state = inc_count state in  
            from <?!>  (Some myaddr, `EchoX (msg, grow) ) >>= continue self state
            
            
        | `GetCount as pmsg->        
          let myaddr = addr self in 
          Lwt.ignore_result @@ Lwt_io.printf ">> Stateful Actor received: %s\n" @@ EchoMessage.show_message pmsg;
          from <?!> (Some myaddr, `Count (safe_get state)) >>= continue self state        
        | _ as pmsg-> 
          Lwt.ignore_result @@ Lwt_io.printf "--- > periodic_actor: Received unexpexted message -- SKIPPING : %s < ---- \n" (EchoMessage.show_message pmsg)
          ; (Lwt.return_unit >>= continue self state)) in 

    let (periodic_actor, p_loop) = spawn ~timeout:(Some (0.25, fun p -> `Timeout p)) (fun self state from -> function 
        | `Echo msg as pmsg -> 
          Lwt.ignore_result @@ Lwt_io.printf ">> Periodc Actor received: %s : %s\n" (EchoMessage.show_message pmsg) msg
          ; continue self state ()
        | (`EchoX (msg, grow) : EchoMessage.message) as pmsg-> 
          let my_addr = addr self in
          Lwt.ignore_result @@ Lwt_io.printf ">> Periodc Actor received: %s\n" @@ EchoMessage.show_message pmsg;
          let c =  String.get msg 0 in 

          let state = apppend_state state (match String.length msg with 
          | 1 ->  (Printf.sprintf "%s%c" msg c, true, from) 
            
          | 10 as len -> 
            let _ = from <?!> (Some my_addr, `GetCount) in
            (String.sub msg 1 (len-1), false, from)         

          | len ->  (match grow with 
            | true ->  (Printf.sprintf "%s%c" msg c, true, from)
              
            | false ->  (String.sub msg 1 (len-1), false, from)))
            in continue self state ()
          
        | `Count _  as pmsg->          
          Lwt.ignore_result @@ Lwt_io.printf ">> Periodc Actor received: %s\n" @@ EchoMessage.show_message pmsg
          ; continue self state ()
        
        | `Timeout n as pmsg-> 
          let my_addr = addr self in
          Lwt.ignore_result @@ Lwt_io.printf ">> Periodc Actor received: %s -- %f \n" (EchoMessage.show_message pmsg)  n;
          (match state with 
          | Some xs -> 
            xs |> List.iter (fun (msg, grow, from) -> let _ = from <?!> (Some my_addr, `MsgX (msg, grow)) in ()) 
            ; continue self (Some []) ()
(* (match state with 
      | Some xs -> 
        xs |> List.iter (fun (msg, grow, from) -> let _ = from <?> (Some self, `MsgX (msg, grow)) in ()) 
        ; continue self (Some []) true *)

          | None -> continue self (Some []) () )
        | `Terminate as pmsg -> 
          Lwt.ignore_result @@ Lwt_io.printf ">> Periodc Actor received: %s\n" @@ EchoMessage.show_message pmsg;
          terminate self state ()
        | `Baz as pmsg ->
          Lwt.ignore_result @@ Lwt_io.printf ">> Periodc Actor received: %s\n" @@ EchoMessage.show_message pmsg;
          continue self (Some []) () 
        | _ as pmsg -> 
          Lwt.ignore_result @@ Lwt_io.printf "--- > periodic_actor: Received unexpexted message -- SKIPPING : %s < ---- \n" (EchoMessage.show_message pmsg)
          ;  continue self state ()) in  
    
    let _ = stateful_actor <!> (Some periodic_actor, `MsgX ("X", true)) in
    let _ = stateful_actor <!> (Some periodic_actor, `MsgX ("Y", true)) in    

    Lwt_main.run @@ Lwt.join [s_loop; p_loop]

let () = test_two ()

(* type sink = int EventStream.Sink.s
type source = int EventStream.Source.s 
type actor = sink * source


let eval ((sink, source), state) ~action ~receive ~after = 
  let pact = action (state) in 
  let msgp = EventStream.Source.get source >|= (function Some v -> `Message v | None -> `NoMessage)
 *)

(* 
let test_new_actor () = 
  let (source, sink) = EventStream.create 1024 in  () *)
  


    (* 
      If the actor was a monad we could do something like this:

      let actor = Actor.create state in 
      actor >>= (receive <|> timeout <|> )   

      Actor.eval actor
      ~action: fun actor -> dest <!> (actor#addr msg) ; return actor
      ~receive: fun actor -> function
      | patter1 -> action1
      | pattern2 -> action2
      | _ ->
      ~receive_when: 
      ~after: time fun actor state -> do somethnig ; return actor stae   
      actor 
      >>= fun self state -> 
        dest <!> msg 
        let rec loop self state = function        
        | 
     *)