
  
  type error_kind = [`NoMsg | `Msg of string | `Code of int | `Pos of (string * int * int * int) | `Loc of string] [@@deriving show]

  type error = [
    | `OutOfBounds of error_kind
    | `OutOfRange of error_kind
    | `IOError of error_kind    
    | `ClosedSession of error_kind
    | `InvalidFormat of error_kind
    | `ProtocolError of error_kind
    | `InvalidSession of error_kind
    | `ResourceLimitViolation of error_kind
    | `InvalidAddress
    | `NotImplemented
    | `UnknownSubMode
    | `UnknownMessageId
    | `ErrorStack of error list ]
    [@@deriving show]


exception Exception of error [@@deriving show]

module Vle = struct
  include Int64

  let of_char =
    let open Common.Infix in
    Int64.of_int <.> int_of_char

  let byte_mask =  0x7fL
  let more_bytes_flag = 0x80L
  let shift_len = 7
  let max_bits = 64
  let max_bytes = 10
end

open Common.Result
open Common.Result.Infix 
let read1_spec log p1 c buf =     
  log ;
  p1 buf 
  >>= (fun (a1, buf) -> 
      return ((c a1),buf))

let read2_spec log p1 p2 c buf =   
  log ;
  p1 buf 
  >>= (fun (a1, buf) -> 
    p2 buf 
    >>= (fun (a2, buf) ->
      return ((c a1 a2),buf)))

let read3_spec log p1 p2 p3 c buf = 
  log ;
  p1 buf 
  >>= (fun (a1, buf) -> 
    p2 buf 
    >>= (fun (a2, buf) ->
      p3 buf 
      >>= (fun (a3, buf) ->
      return ((c a1 a2 a3),buf))))

let read4_spec log p1 p2 p3 p4 c buf = 
  log ;
  p1 buf 
  >>= (fun (a1, buf) -> 
    p2 buf 
    >>= (fun (a2, buf) ->
      p3 buf 
      >>= (fun (a3, buf) ->
        p4 buf 
        >>= (fun (a4, buf) ->
        return ((c a1 a2 a3 a4),buf)))))

let read5_spec log p1 p2 p3 p4 p5 c buf = 
  log ;
  p1 buf 
  >>= (fun (a1, buf) -> 
    p2 buf 
    >>= (fun (a2, buf) ->
      p3 buf 
      >>= (fun (a3, buf) ->
        p4 buf 
        >>= (fun (a4, buf) ->
          p5 buf 
          >>= (fun (a5, buf) ->
        return ((c a1 a2 a3 a4 a5),buf))))))

