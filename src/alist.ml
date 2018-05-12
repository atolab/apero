open Amonad

module List = struct
  include List

  let drop = Acommon.drop

  let take = Acommon.take

  let zip = combine

  let unzip = split

  let  to_string ?(sep=", ") xs f =
    let s = match xs with
      | h::tl -> List.fold_left (fun a v -> Printf.sprintf "%s%s%s" a sep @@ f v) (f h) tl
      | [] -> ""
    in "(" ^ s ^ ")"
            
end

module ListM =
  MakePlus(struct
    type 'a m = 'a list
    let return x  = [x]
    let bind xs f = List.concat (List.map f xs)
    let zero  () = []
    let plus xs ys = xs @ ys
    let null = function
        [] -> true
      | _  -> false
  end)
