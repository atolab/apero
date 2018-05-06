
module List = struct
  include List

  let drop = Common.drop

  let take = Common.take

  let zip = combine

  let unzip = split

end

module ListM =
  Monad.MakePlus(struct
    type 'a m = 'a list
    let return x  = [x]
    let bind xs f = List.concat (List.map f xs)
    let zero  () = []
    let plus xs ys = xs @ ys
    let null = function
        [] -> true
      | _  -> false
  end)
