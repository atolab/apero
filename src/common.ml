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
