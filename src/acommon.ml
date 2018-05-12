
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
