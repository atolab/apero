open Actor
open Common.Infix
open Lwt
open Lwt.Infix

exception Terminate

type 't task = 't -> 't Lwt.t

let spawn ?(queue_len=256) ?(timeout=None) ?(on_terminate=None) state pack unpack = 
  let state = Some(state |> pack |> return) in

  let timeout = match timeout with 
  | None -> None 
  | Some (period, make_timeout) -> Some(period, fun fl -> (fun t -> t |> unpack %> make_timeout fl >>= pack %> return)) in

  let on_terminate = match on_terminate with 
  | None -> None 
  | Some func -> Some(fun u -> (fun t -> t |> unpack %> func u >>= pack %> return)) in

  let (mailbox, loop) = Actor.spawn ~queue_len ~state ~timeout ~on_terminate (fun self state _ f ->
    let%lwt state = Common.Option.get state in
    try%lwt 
      let%lwt new_state = f state in
      Actor.continue self (Some (Lwt.return new_state)) () 
    with  
    | Terminate -> Actor.terminate self (Some (Lwt.return state)) ()
    | e ->  let msg = Printexc.to_string e
            and stack = Printexc.get_backtrace () in
            let%lwt _ = Logs_lwt.warn (fun m -> m "Fnactor got exception : %s\n%s" msg stack) in
            Actor.continue self (Some (Lwt.return state)) ()
    ) in 

  let push = fun func -> Actor.send mailbox None (fun state -> state |> unpack %> return >>= func >>= pack %> return) in 
  
  (push, loop)

let terminate _ = raise Terminate  (* TODO : Fail Terminate*)

let pure f = fun t -> let r = f in  Lwt.return(r, t)
let readonly f = fun t -> let%lwt r = f t in Lwt.return (r, t)

let (@%>) f g = fun t -> let (t, r) = f t in let _ = g r in t
let (%@>) f g = fun t -> let (r, t) = f t in let _ = g r in t

let (@%>>) f g = fun t -> let%lwt (t, r) = f t in let _ = g (Lwt.return r) in Lwt.return t
let (%@>>) f g = fun t -> let%lwt (r, t) = f t in let _ = g (Lwt.return r) in Lwt.return t

let (@%>>=) f g = fun t -> let%lwt (t, r) = f t in let _ = g r in Lwt.return t
let (%@>>=) f g = fun t -> let%lwt (r, t) = f t in let _ = g r in Lwt.return t
