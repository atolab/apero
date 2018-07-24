open Fnactor
open Common.Infix
open Printf

module A = struct
  type t = { count: int; op: string }

  let add i t =
    printf "ADD %d (%d, %s)\n%!" i t.count t.op;
    {count=t.count+i; op=t.op^"+"^(string_of_int i)}

  let sub i t =
    printf "SUB %d (%d, %s)\n%!" i t.count t.op;
    {count=t.count-i; op=t.op^"-"^(string_of_int i)}

  let div i t = 
    printf "DIV %d (%d, %s)\n%!" i t.count t.op;
    ({count=t.count/i; op=t.op^"/"^(string_of_int i)}, t.count mod i)    

  let get t = 
    printf "GET (%d, %s)\n%!" t.count t.op;
    (t.count)
end

open A

type states = 
  | A of A.t
  | B of int

let pack_a x = A(x)
let unpack_a = function | A x -> x | _ -> failwith "ERROR"

let pack_b x = B(x)
let unpack_b = function | B x -> x | _ -> failwith "ERROR"

let (actor_a, a_loop) = spawn {count=0; op=""} pack_a unpack_a
let (actor_b, b_loop) = spawn {count=0; op=""} pack_a unpack_a
let (actor_c, c_loop) = spawn 0 pack_b unpack_b

(* In this example ignore failures when sending message (full queue) *)
let actor_a = actor_a %> Lwt.ignore_result
let actor_b = actor_b %> Lwt.ignore_result
let actor_c = actor_c %> Lwt.ignore_result

let main () =

  actor_a @@ add 100;

  actor_a @@ div 3 @%> ignore;

  actor_a @@ div 2 @%> (fun r -> actor_c @@ (fun t -> printf "  REPLY %d (%d)\n%!" r t; t));
  actor_a @@ div 3 @%> (fun r -> actor_c @@ readonly (printf "  REPLY %d (%d)\n%!" r) @%> ignore);
  actor_a @@ div 3 @%> (fun r -> actor_c @@ pure (printf "  REPLY %d\n%!" r) @%> ignore);

  actor_a @@ div 2 @%> (fun r -> actor_b @@ div r @%> (fun r2 -> actor_c @@ pure (printf "  REPLY %d\n%!" r2) @%> ignore));

  actor_a @@ readonly get @%> ignore;

  actor_a @@ readonly get @%> (fun r -> actor_c @@ pure (printf "  REPLY %d\n%!" r) @%> ignore);

  actor_a @@ pure (Unix.sleep 1) @%> ignore;

  actor_a @@ (fun t -> printf "WHATEVER\n%!"; t);

  actor_a @@ terminate;
  actor_b @@ terminate;
  actor_c @@ terminate;

  Lwt.return_unit

let () = Lwt_main.run @@  Lwt.join [main () ; a_loop; b_loop; c_loop; ]
