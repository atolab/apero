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
  | `InvalidFlags
  | `NotImplemented
  | `UnknownSubMode
  | `UnknownMessageId
  | `UnexpextedMessage
  | `ValidationError of error_kind
  | `ErrorStack of error list ]
  [@@deriving show]


(* exception Exception of error [@@deriving show] *)
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
