open Actor

exception Terminate

let spawn ?(queue_len=256) ?(timeout=None) ?(on_terminate=None) state pack unpack = 
  let state = Some(pack (state)) in

  let timeout = match timeout with 
  | None -> None 
  | Some (period, make_timeout) -> Some(period, fun fl -> (fun t -> unpack t |> make_timeout fl |> pack)) in

  let on_terminate = match on_terminate with 
  | None -> None 
  | Some func -> Some(fun u -> (fun t -> unpack t |> func u |> pack)) in

  let (mailbox, loop) = Actor.spawn ~queue_len ~state ~timeout ~on_terminate (fun self state _ f ->
    let state = Common.Option.get state in
    try Actor.continue self (Some (f state)) () 
    with  
    | Terminate -> Actor.terminate self (Some state) ()
    | e ->  let msg = Printexc.to_string e
            and stack = Printexc.get_backtrace () in
            Lwt.ignore_result @@ Logs_lwt.warn (fun m -> m "Fnactor got exception : %s\n%s" msg stack);
            Actor.continue self (Some state) ()
    ) in 

  let push = fun func -> Actor.send mailbox None (fun state -> unpack state |> func |> pack) in 
  
  (push, loop)

let terminate _ = raise Terminate 

let pure f = fun t -> let r = f in (r, t)

let readonly f = fun t -> let r = f t in (r, t)

let (@%>) f g = fun t -> let (t, r) = f t in let _ = g r in t
let (%@>) f g = fun t -> let (r, t) = f t in let _ = g r in t
