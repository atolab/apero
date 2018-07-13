open Apero
open Lwt.Infix


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
    let stateful_octar = spawn  ~state:(Some 0) (fun self state from -> function 
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

    let periodic_actor = spawn ~timeout:(Some 0.001) (fun self state from -> function 
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
    | `Baz -> continue self (Some []) true 
    | _ -> 
      Lwt.ignore_result @@ Lwt_io.printf "--- > periodic_actor: Received unexpexted message -- SKIPPING < ---- \n" 
      ; Lwt.return_true >>= continue self state) in  
    
    let _ = stateful_octar <!> (Some periodic_actor, `MsgX ("X", true)) in
    let _ = stateful_octar <!> (Some periodic_actor, `MsgX ("Y", true)) in
    Lwt_main.run @@ run_loop stateful_octar

    let () = test_two ()