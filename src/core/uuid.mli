module Uuid : sig  
  type t 
  
  val make : unit -> t  
    
  val next_id : unit -> t 
  
  val compare : t -> t -> int 
  val equal : t -> t -> bool
  
  val of_bytes : ?pos:int -> string -> t option
  val to_bytes : t -> string

  val of_string : ?pos:int -> string -> t option
  val to_string : ?upper:bool -> t -> string
  
end 
