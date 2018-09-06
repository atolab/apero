open Apero


type message =
  | Msg of string * Lwt_io.input Lwt_io.channel * Lwt_io.output Lwt_io.channel
  | IMsg of int * Lwt_io.input Lwt_io.channel * Lwt_io.output Lwt_io.channel
  [@deriving show]


open Lwt.Infix
(* open Actor  *)

let rec handle_connection ic oc worker_actor client_actor ()=
  let open Actor.Infix in 
  Lwt_io.read_line_opt ic >>=
  (fun msg ->
     match msg with
     | Some msg ->
       let _ = (worker_actor <!> (Some client_actor, Msg (msg, ic, oc)) ) in
       handle_connection ic oc worker_actor client_actor ()
     | None ->   Logs_lwt.info (fun m -> m "Connection closed" >>= Lwt.return ))


let accept_connection worker_actor connection_actor conn =
  let fd, _ = conn in
  let ic = Lwt_io.of_fd ~mode:Lwt_io.Input fd in
  let oc = Lwt_io.of_fd ~mode:Lwt_io.Output fd in
  Lwt.on_failure (handle_connection ic oc worker_actor connection_actor ()) (fun e -> Logs.debug (fun m -> m "%s" @@ Printexc.to_string e));
  Logs_lwt.info (fun m -> m "New connection") >>= Lwt.return

let create_socket host port backlog () =
  let open Lwt_unix in
  let sock = socket PF_INET SOCK_STREAM 0 in
  Lwt_unix.setsockopt sock SO_REUSEADDR true;
  let _ = Lwt_unix.bind sock (ADDR_INET(host, port)) in
  Lwt_unix.listen sock backlog;
  sock

let create_server sock worker_actor  connection_actor =
  let rec serve () =
    Lwt_unix.accept sock >>= accept_connection worker_actor connection_actor >>= serve
  in serve

open Actor
let (echo_actor, ea_loop) = spawn (fun self state from ->
    function
    | Msg (msg, ic , oc) ->
      ignore @@ Lwt_io.printf "[ea] Received %s\n" msg;
      let r = IMsg ((Random.int 512), ic, oc) in
      let r2 = (maybe_send from None r) in
      r2 >>= (continue self state)
    | IMsg (imsg, ic, oc) ->
      ignore @@ Lwt_io.printf "[ea] Received %d\n" imsg;
      let r = Msg (("A"^ string_of_int @@ (Random.int 512)), ic, oc) in
      maybe_send from None r >>= continue self state
  )

let (client_actor, c_loop) = spawn (fun self state _ ->
    function
    | Msg (msg, _ , oc) ->
      ignore @@ Lwt_io.printf "[ca] Received %s\n" msg;
      let _ = Lwt_io.write_line oc msg in
      continue self state ()
    | IMsg (imsg, _, oc) ->
      ignore @@ Lwt_io.printf "[ca] Received %d\n" imsg;
      let _ = Lwt_io.write_line oc (string_of_int imsg) in
      continue self state ()
  )

let main () =
  Random.self_init();
  let%lwt _ = Lwt_io.printf "starting server..." in
  let backlog = 10 in
  let port = 5000 in
  let host = Unix.inet_addr_loopback in
  let sock = create_socket host port backlog () in
  let serve = create_server sock echo_actor client_actor in
  serve ()

let () =
  
  let server = main () in
  Lwt_main.run @@  Lwt.join [server ; ea_loop; c_loop; ]