open Apero

(* module Message = struct 
    type message = [
        `Msg of string * Lwt_io.input Lwt_io.channel * Lwt_io.output Lwt_io.channel
        |`IMsg of int * Lwt_io.input Lwt_io.channel * Lwt_io.output Lwt_io.channel
        | TActor.core_message
    ]
    let timeout p = `Timeout p
end *)

(* module EchoActor = TActor.Make(Message) *)

type message = [
        `Msg of string * Lwt_io.input Lwt_io.channel * Lwt_io.output Lwt_io.channel
        |`IMsg of int * Lwt_io.input Lwt_io.channel * Lwt_io.output Lwt_io.channel
        | TActor.core_message
    ]


open Actor
open Lwt.Infix

let rec handle_connection ic oc worker_actor client_actor ()= 
    Lwt_io.read_line_opt ic >>= 
      (fun msg -> 
       match msg with
       | Some msg -> 
            let _ = (worker_actor <!> (Some client_actor, `Msg (msg, ic, oc)) ) in
            handle_connection ic oc worker_actor client_actor ()
       | None ->   Lwt_log.info "Connection closed" >>= Lwt.return )


let accept_connection worker_actor connection_actor conn =
    let fd, _ = conn in
    let ic = Lwt_io.of_fd ~mode:Lwt_io.Input fd in
    let oc = Lwt_io.of_fd ~mode:Lwt_io.Output fd in
    Lwt.on_failure (handle_connection ic oc worker_actor connection_actor ()) (fun e -> Lwt_log.ign_error (Printexc.to_string e));
    Lwt_log.info "New connection" >>= Lwt.return
    
let create_socket host port backlog () =
    let open Lwt_unix in
    let sock = socket PF_INET SOCK_STREAM 0 in
    Lwt_unix.setsockopt sock SO_REUSEADDR true;
    let _ = Lwt_unix.bind sock (ADDR_INET(host, port)) in
    Lwt_unix.listen sock backlog;
    sock

let create_server sock worker_actor connection_actor =
    let rec serve () =
    Lwt_unix.accept sock >>= accept_connection worker_actor connection_actor >>= serve
    in serve

let echo_actor = spawn (fun self state from -> 
        function 
        | `Msg (msg, ic , oc) -> 
            ignore @@ Lwt_io.printf "[ea] Received %s\n" msg;
            let r = `IMsg ((Random.int 512), ic, oc) in 
            maybe_send from None r >>= continue self state
        | `IMsg (imsg, ic, oc) ->
            ignore @@ Lwt_io.printf "[ea] Received %d\n" imsg;
            let r = `Msg (("A"^ string_of_int @@ (Random.int 512)), ic, oc) in 
            maybe_send from None r >>= continue self state
        | _ -> continue self state true
    )

let client_actor = spawn (fun self state from -> 
        function 
        | `Msg (msg, _ , oc) -> 
            ignore @@ Lwt_io.printf "[ca] Received %s\n" msg;
            let _ = Lwt_io.write_line oc msg in
            continue self state true
         | `IMsg (imsg, _, oc) ->
            ignore @@ Lwt_io.printf "[ca] Received %d\n" imsg;
            let _ = Lwt_io.write_line oc (string_of_int imsg) in
            continue self state true
        | _ -> continue self state true
    )

let main () = 
     Random.self_init();
 
    let backlog = 10 in 
    let port = 5000 in 
    let host = Unix.inet_addr_loopback in
    Lwt_log.add_rule "*" Lwt_log.Info;
    let sock = create_socket host port backlog () in
    let serve = create_server sock echo_actor client_actor in
    let _ = serve () in 
    echo_actor


let () =
    Lwt_main.run @@ Actor.run_loop (main ())