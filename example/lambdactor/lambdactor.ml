open Actor


type t = { count: int; op: string }


let add i t =
  Lwt.ignore_result @@ Lwt_io.printf "   - ADD %d to %d\n" i t.count;
  (t.count+i, {count=t.count+i; op=t.op^"+"^(string_of_int i)} )

let sub i t =
  Lwt.ignore_result @@ Lwt_io.printf "   - SUB %d to %d\n" i t.count;
  (t.count-i, {count=t.count-i; op=t.op^"-"^(string_of_int i)} )

let get_op t = (t.op, t)


type ('result,'caller_state) function_call =
  | Call of { f : t -> ('result * t); on_return: 'result -> 'caller_state -> 'caller_state }
  | Return of { f : 'caller_state -> 'caller_state }



let test_actor (init_state:t) = 
  let open Actor in
  let open Lwt.Infix in
  spawn ~state:(Some init_state) (fun self state from ->
      let current_state =
        match state with
        | Some(s) -> s
        | None -> failwith "!!!!! Error state must be present" in
      Lwt.ignore_result @@ Lwt_io.printf "TEST_ACTOR RECV MSG...\n";
      function
      | Call{f; on_return} ->
        Lwt.ignore_result @@ Lwt_io.printf "DO CALL...\n";
        let (result, new_state) = f current_state in
        Lwt.ignore_result @@ Lwt_io.printf "  state after call = %d\n" new_state.count;
        let response = Return{f=on_return result} in
        maybe_send from None response >>= continue self (Some new_state)
      | Return{f} ->
        Lwt.ignore_result @@ Lwt_io.printf "DO ON_RETURN...\n";
        let new_state = f current_state in
        Lwt.ignore_result @@ Lwt_io.printf "  state after on_return = %d\n" new_state.count;
        continue self (Some new_state) true
    )


let create_test () = 
  Lwt.ignore_result @@ Lwt_io.printf "TEST CREATE\n";
  let init_state = { count=0; op="0" } in
  let mm_actor,mm_loop = test_actor init_state in
  mm_actor,mm_loop


let run () =
  let open Actor in 
  let (test, test_loop) = create_test () in
  let _ =
    Lwt.ignore_result @@ Lwt_io.printf "\n---- CALLING ADD 5\n";
    test <!>
    (None, 
     Call{
       f=add 5;
       on_return=fun i x ->
         Lwt.ignore_result @@ Lwt_io.printf "---- RESULT = %d \n" i;
         x
     }) in
  let _ =
    Lwt.ignore_result @@ Lwt_io.printf "\n---- CALLING SUB 10\n";
    test <!>
    (None, 
     Call{
       f=sub 10;
       on_return=fun i x ->
         Lwt.ignore_result @@ Lwt_io.printf "---- RESULT = %d \n" i;
         x
     }) in
  (***** ERROR WHEN ADDING FOLLOWING CODE *****) 
  let _ =
    Lwt.ignore_result @@ Lwt_io.printf "\n---- CALLING GET_OP\n";
    test <!>
    (None, 
     Call{
       f=get_op;
       on_return=fun s x ->
         Lwt.ignore_result @@ Lwt_io.printf "---- RESULT = %s \n" s;
         x
     }) in
  Lwt.ignore_result @@ Lwt_io.printf "!!!! DO JOIN \n";
  Lwt_main.run @@ Lwt.join [test_loop];
  Lwt.ignore_result @@ Lwt_io.printf "!!!! JOIN FINISH \n"

let () = run ()
