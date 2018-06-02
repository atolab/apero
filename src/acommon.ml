
let drop n xs =
  let rec adrop n xs = match n with
    | 0 -> xs
    | _ -> match xs with
      | [] -> []
      | _::tl -> adrop (n-1) tl
  in adrop n xs

let take n xs =
  let rec atake n xs ys = match n with
    | 0 -> List.rev ys
    | _ -> match xs with
      | [] -> List.rev ys
      | h::tl -> atake (n-1) tl  (h::ys)
  in atake n xs []


let compose f g = fun x -> f @@ g @@ x


let some default f o = match o with
  | Some v -> f v
  | _ -> default

let result f g r = match r with
  | Ok v -> f v
  | Error e -> g e

let apply f v = f v

let flip f = fun a b -> f b a 

let apply_n (t : 'a) (f : 'a -> 'b)  (n: int) =
  let rec loop_n n xs =
    if n = 1 then xs
    else loop_n (n-1) ((f t) :: xs)
  in loop_n n []


module InfixM = struct
  let (<.>) = compose
  let (<*>) f v = f v
end


module OptionM = struct 
  let bind o f = match o with 
  | Some v -> f v
  | None -> None

  let return v = Some v

  let zero () = None

  let is_some = function
  | Some _ -> true
  | _ -> false 

  let get = function 
  | Some v -> v 
  | _ -> failwith "Can't get value out of None"

  let get_or_else opt f = match opt with
  | Some v  -> v 
  | _ -> f ()

  let or_else opt (f: unit -> 'a option) = match opt with 
  | Some _ -> opt 
  | None -> f ()

  let iter opt f = match opt with
  | Some v -> f v 
  | _ -> ()

  let flatten os = 
    let rec rflat os fos = match os with 
    | h::tl -> (match h with 
      | Some v -> rflat tl (v::fos)
      | None -> rflat tl fos)
    | [] -> Some fos
    in rflat os []

  let lift f = function 
  | Some x -> Some (f x)
  | None -> None

  module InfixM = struct 
    let (>>=) = bind
    let (<$>) = lift
  end
  
  
end

module ResultM = struct
  type (+'a, +'e) t = ('a, 'e) result
  
  let bind r  f = match r with 
  | Ok v -> f v 
  | Error e -> Error e

  let bind_error r f = match r with 
  |Error e -> f e
  | Ok _ as ok -> ok

  let return v = Ok v

  let ok = return

  let fail e = Error e 

  let is_ok = function 
  | Ok _ -> true
  | _ -> false

  let is_error r = not @@ is_ok r

  let get = function 
  | Ok v -> v
  | Error _ -> failwith "Cannot extract result from error!"

  let get_or_else r f = match r with
  | Ok v -> v 
  | Error e -> f e

  let or_else r f = match r with 
  | Ok v -> r 
  | Error e -> f e

  let flatten rs = 
    let rec rflat rs frs = 
      match rs with 
      | h::tl -> (match h with 
        | Ok v -> rflat tl (v::frs) 
        | Error e as err-> err)
      | [] -> Ok frs
    in rflat rs []

    let to_option = function
    | Ok v -> OptionM.return v
    | Error _ -> OptionM.zero ()
  
    let iter r f = match r with 
    | Ok v -> f v
    | Error _  -> ()

    let lift f = function 
    | Ok v -> Ok (f v)
    | Error _ as e -> e 

    module InfixM = struct
      let (>>=) = bind
      let (>>=!) = bind_error

      let (<$>) = lift
    end

end


module LwtM = struct
  include Lwt
  
  let rec fold_m f b xs = match xs with
    | [] -> Lwt.return b
    | h::tl -> f b h >>= (fun b -> fold_m f b tl)

  let lift f =  fun x -> bind x (fun y -> return (f  y))
    
  module InfixM = struct 
    let (<$>) = lift
    let (>>=) = bind
  end
end

module type MonadM = sig 
  type 'a m
  val return : 'a -> 'a m  
  val bind : 'a m -> ('a -> 'b m) -> 'b m
  val map : 'a m -> ('a -> 'b) -> 'b m
  val lift : ('a -> 'b) -> ('a m -> 'b m)
  val iter : 'a m -> ('a -> unit) -> unit
  (* val flatten : ('a m) list -> ('a list) m   *)
  module InfixM : sig
    val (<$>) : ('a -> 'b) -> ('a m -> 'b m)    
    val (>>=) : 'a m -> ('a -> 'b m) -> 'b m
  end 
end
(*  
 module ResultLwtM = struct 
  let lift f = (LwtM.lift (ResultM.lift f))

  let bind m f = LwtM.bind m (fun r -> OptionM.bind r f)

  
  module InfixM = struct 
    let (<$>) = lift
  end 

 end

 


module ComposeM (M : MonadM ) (N : MonadM) : (MonadM with type 'a m := 'a M.m N.m) = struct
  let return x = N.return (M.return x)
  let apply n f = N.apply n (fun m -> M.apply m f)
  let bind  n f = N.map n (fun m -> M.bind m f)
  let map n f = N.map n (fun m -> M.map m f)
  let lift f = N.lift (M.lift f)
  let iter n f = N.iter n (fun m -> M.iter m f)
  
  module  InfixM = struct
    let (<$>) = lift 
    let (>>=) = bind
  end 
end
   *)