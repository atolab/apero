open Apero.StateP
open Apero.StateP.Infix

type game_state = bool * int

let rec play_game = function 
| [] -> read >>= (fun (_, score) -> return score)
| (x::xs) ->  
    read 
    >>= (fun (on, score) -> 
        match x with 
        | 'a' when on -> write (on, score + 1)
        | 'b' when on -> write(on, score - 1)
        | 'c' -> write (not on, score)
        | _ -> write (on, score)) 
        >>= (fun _ -> play_game xs)
  

let () = 
    let s0 : game_state = (true, 0) in 
    let r = eval (play_game ['a';'a';'c';'b';'c';'a';'a';'a';'a';'b']) s0 in 
    Printf.printf "Eval : %d\n" r 