open Actor

exception Terminate

let spawn ?(queue_len=256) ?(timeout=None) ?(on_terminate=None) state pack unpack = 
  let state = Some(pack (state)) in
  let (mailbox, loop) = Actor.spawn ~queue_len ~state ~timeout ~on_terminate (fun self state _ f ->
    let state = Common.Option.get state in
    try Actor.continue self (Some (f state)) () 
    with  | Terminate -> Actor.terminate self (Some state) ()
          | e -> let msg = Printexc.to_string e
            and stack = Printexc.get_backtrace () in
            Lwt.ignore_result @@ Logs_lwt.warn (fun m -> m "Fnactor got exception : %s\n%s" msg stack);
            Actor.continue self (Some state) ()
    ) in 
  let push = fun func -> Lwt.ignore_result @@ Actor.send mailbox None (fun state -> unpack state |> func  |> pack) in
  (push, loop)

let terminate _ = raise Terminate 

let pure f = fun s -> let r = f in (s, r)

let readonly f = fun s -> let r = f s in (s, r)

let (@%>) f g = fun s -> let (s, r) = f s in g r; s
let (%@>) f g = fun s -> let (r, s) = f s in g r; s
