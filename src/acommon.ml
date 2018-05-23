
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

let  (<.>) = compose

let some default f o = match o with
  | Some v -> f v
  | _ -> default

let result f g r = match r with
  | Ok v -> f v
  | Error e -> g e

let apply_n (t : 'a) (f : 'a -> 'b)  (n: int) =
  let rec loop_n n xs =
    if n = 1 then xs
    else loop_n (n-1) ((f t) :: xs)
  in loop_n n []


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

  module Infix = struct 
    let (>>=) = bind
  end
  
  
end

module ResultM = struct
  let bind r f = match r with 
  | Ok v -> f v 
  | _ as e -> e

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

    module Infix = struct
      let (>>=) = bind
      let (>>=!) = bind_error
    end

end