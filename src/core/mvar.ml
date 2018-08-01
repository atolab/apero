module type MVar = sig 
  type 'a t
  val create : 'a -> 'a t
  val create_empty : unit -> 'a t
  val put : 'a t -> 'a -> unit Lwt.t
  val take : 'a t -> 'a Lwt.t
  val take_available : 'a t -> 'a option
  val is_empty : 'a t -> bool
  
  val read : 'a t -> 'a Lwt.t  
  (* [read] provides a reference to the value stored on the mvar
     that is safe for read-only operation. This allows to have
     read/write concurrency while leveraging functional data 
     structures
  *)
 val guarded : 'a t -> ('a -> (('b Lwt.t * 'a) Lwt.t)) -> 'b Lwt.t
  (* [guarded] execute a function that changes the state and returns
     a result promise while ensuring the the mvar is acquired before 
     executing the function and released after even if exceptions
     are raised while running the function *)
  
  val guarded_and_then : 'a t -> ('a -> (('b Lwt.t * 'a) Lwt.t)) -> ('a -> 'b Lwt.t -> 'c Lwt.t) -> 'c Lwt.t
  (* [guarded_and_then m f g] runs  f as "guarded m f" but progagates the result and the new state
    to execute g. Notice that g cannot change the state thus is a read-only operation *)
end

module MVar_lwt = struct
  (* This module wraps the Lwt_mvar implementation *)  
  include Lwt_mvar  

  let read m =     
    let open Lwt.Infix in 
    (take m) >>= fun v -> (put m v) >|= fun () -> v
  
  let guarded m f = 
    let open Lwt.Infix in 
    let%lwt s = take m in  
    try%lwt 
      f s >>= fun (r, s') -> put m s' >>= fun () -> r
    with 
    | _ as e -> 
      put m s >>= fun _ -> Lwt.fail e

  let guarded_and_then m f g = 
    let open Lwt.Infix in 
    let%lwt s = take m in 
    try%lwt 
      f s >>= fun (r, s') ->
        let%lwt _ = put m s' in
        try%lwt 
         g s' r
        with
        | _ as e -> Lwt.fail e
        
    with 
    | _ as e -> put m s >>= fun _ -> Lwt.fail e
    

end  