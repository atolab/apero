val drop : int -> 'a list -> 'a list
val take : int -> 'a list -> 'a list
val compose : ('b -> 'c) -> ('a -> 'b) -> ('a -> 'c)

val (<.>) : ('b -> 'c) -> ('a -> 'b) -> ('a -> 'c)

val some : 'b -> ('a -> 'b) -> 'a option -> 'b

val result : ('a -> 'c) -> ('b -> 'c) -> ('a, 'b) result -> 'c

val apply_n : 'a -> ('a -> 'b) -> int  -> 'b list

module OptionM : sig
  val bind : 'a option -> ('a -> 'b option) -> 'b option 
  val return : 'a -> 'a option
  val zero : unit -> 'a option
  val is_some : 'a option -> bool
  val get : 'a option -> 'a
  val get_or_else : 'a option ->  (unit -> 'a) ->  'a
  val or_else : 'a option -> (unit -> 'a option) -> 'a option
  val flatten : ('a option) list -> ('a list) option
  val iter : 'a option -> ('a -> unit) -> unit
  module Infix : sig 
    val (>>=) : 'a option -> ('a -> 'b option) -> 'b option 
  end
end

module ResultM : sig 
  val bind : ('a, 'b) result -> ('a -> ('a, 'b) result) -> ('a,'b) result 
  val bind_error : ('a, 'b) result -> ('b -> ('a, 'b) result) -> ('a,'b) result 

  val return : 'a -> ('a, 'b) result
  val ok : 'a -> ('a, 'b) result
  val fail : 'b -> ('a, 'b) result
  val is_ok : ('a, 'b) result -> bool
  val is_error : ('a, 'b) result -> bool
  val get : ('a, 'b) result -> 'a
  val get_or_else : ('a, 'b) result -> ('b -> 'a) -> 'a
  val or_else : ('a, 'b) result  -> ('b -> ('a, 'b) result) -> ('a, 'b) result
  val flatten : (('a, 'b) result) list -> ('a list, 'b) result
  val iter : ('a, 'b) result -> ('a -> unit) -> unit
  val to_option : ('a, 'b) result -> 'a option

  module Infix : sig 
    val (>>=) : ('a, 'b) result -> ('a -> ('a, 'b) result) -> ('a,'b) result 
    val (>>=!) : ('a, 'b) result -> ('b -> ('a, 'b) result) -> ('a,'b) result 
  end
end