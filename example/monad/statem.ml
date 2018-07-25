open State


module GameState = State.Make(struct type s = bool * int end)

open GameState
open GameState.Infix

let rec play_game = function 
| [] -> read >>= (fun (_, score) -> return score)
| (x::xs) -> let r = bind read (fun (on, score) -> 
    match x with 
    | 'a' when on -> write (on, score + 1)
    | 'b' when on -> write(on, score - 1)
    | 'c' -> write (not on, score)
    | _ -> write (on, score)) in 
     r >>= (fun _ -> play_game xs)
  


let () = 
  let r = eval (play_game ['a';'a';'c';'b';'c';'a';'a';'a';'a';'b']) (true, 0) in 
  Printf.printf "Eval : %d\n" r 