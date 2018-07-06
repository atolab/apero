val drop : int -> 'a list -> 'a list
val take : int -> 'a list -> 'a list
val compose : ('b -> 'c) -> ('a -> 'b) -> ('a -> 'c)
val some : 'b -> ('a -> 'b) -> 'a option -> 'b
val apply : ('a -> 'b) -> 'a -> 'b
val apply_n : 'a -> ('a -> 'b) -> int  -> 'b list
val flip : ('a -> 'b -> 'c) -> ('b -> 'a -> 'c)
module InfixM : sig 
  val (<.>) : ('b -> 'c) -> ('a -> 'b) -> ('a -> 'c)
  val (<*>) : ('a -> 'b) -> 'a -> 'b
end

module type MonadM = sig 
  type 'a m
  val return : 'a -> 'a m  
  val bind : 'a m -> ('a -> 'b m) -> 'b m
  val map : 'a m -> ('a -> 'b) -> 'b m
  val lift : ('a -> 'b) -> ('a m -> 'b m)
  val iter : 'a m -> ('a -> unit) -> unit
  (* val flatten : ('a m) m -> 'a m   *)
  module  InfixM : sig
    val (<$>) : ('a -> 'b) -> ('a m -> 'b m)
    val (>>=) : 'a m -> ('a -> 'b m) -> 'b m
  end 
end

module OptionM : sig
  val bind : 'a option -> ('a -> 'b option) -> 'b option 
  val return : 'a -> 'a option
  val zero : unit -> 'a option
  val is_some : 'a option -> bool
  val get : ?if_none:'a ->'a option ->  'a
  val get_or_else : 'a option ->  (unit -> 'a) ->  'a
  val or_else : 'a option -> (unit -> 'a option) -> 'a option
  val flatten : ('a option) list -> ('a list) option
  val iter : 'a option -> ('a -> unit) -> unit
  val lift : ('a -> 'b) -> ('a option -> 'b option)
  module InfixM : sig 
    val (>>=) : 'a option -> ('a -> 'b option) -> 'b option 
    val (<$>) : ('a -> 'b) -> ('a option -> 'b option)
    val (>==) : 'a option -> ('a -> 'b) -> 'b option
  end
end

module ResultM : sig 
  type (+'a, +'e) t = ('a, 'e) result

  val bind : ('a, 'e) t -> ('a -> ('c, 'e) t) -> ('c,'e) t
  val bind2 : (('a * 'b), 'e) t -> ('a -> 'b -> ('c, 'e) t) -> ('c,'e) t
  val map : ('a, 'e) t -> ('a -> 'c) -> ('c,'e) t
  val bind_error : ('a, 'e) t -> ('e -> ('a, 'i) t) -> ('a,'i) t

  val fold_m : ('a -> 'b -> ('b, 'e) t) -> 'a list -> 'b -> ('b, 'e) t

  val return : 'a -> ('a, 'e) t
  val ok : 'a -> ('a, 'e) t
  val fail : 'e -> ('a, 'e) t
  val is_ok : ('a, 'e) t -> bool
  val is_error : ('a, 'e) t -> bool
  val get : ('a, 'e) t -> 'a
  val try_get :  run:('a -> 'c) -> fail_with: ('e -> 'c) -> on:('a, 'e) t -> 'c
  val get_or_else : ('a, 'e) t -> ('e -> 'a) -> 'a
  val or_else : ('a, 'e) t  -> ('e -> ('a, 'e) t) -> ('a, 'e) t
  val flatten : (('a, 'e) t) list -> ('a list, 'e) t
  val iter : ('a, 'e) t -> ('a -> unit) -> unit
  val to_option : ('a, 'e) t -> 'a option
  val lift : ('a -> 'b) -> ('a, 'e) t -> ('b, 'e) t
  val cons : ('a, 'e) t -> ('a list, 'e) t -> ('a list, 'e) t 

  module InfixM : sig   
    val (>>=) : ('a, 'e) t -> ('a -> ('c, 'e) t) -> ('c,'e) t 
    val (>>==): (('a * 'b), 'e) t -> ('a -> 'b -> ('c, 'e) t) -> ('c,'e) t
    val (>>>) : ('a, 'e) t -> ('a -> 'c) -> ('c,'e) t
    val (>>=!) : ('a, 'e) t -> ('e -> ('a, 'i) t) -> ('a,'i) t 
    val (<$>) :  ('a -> 'b) -> ('a, 'e) t -> ('b, 'e) t
  end
end

module LwtM : sig  
  include (module type of Lwt)
  val fold_m : ('a -> 'b -> 'a Lwt.t) -> 'a -> 'b list  -> 'a Lwt.t    
  val lift : ('a -> 'b) -> ('a Lwt.t -> 'b Lwt.t)

  val flatten : ('a Lwt.t) list -> ('a list) Lwt.t 

  module InfixM : sig 
    val (<$>) : ('a -> 'b) -> ('a Lwt.t -> 'b Lwt.t)
    val (>>=) : 'a Lwt.t -> ('a -> 'b Lwt.t) ->  'b Lwt.t
  end
end

(* module ResultLwtM : sig 
   val lift : ('a -> 'b) -> ('a, 'c) result Lwt.t -> ('b, 'c) result Lwt.t 
   val bind : ('a, 'c) result Lwt.t -> ('a -> ('b, 'c) result Lwt.t) -> ('b, 'c) result Lwt.t

   module InfixM : sig 
   val (<$>) : ('a -> 'b) -> ('a, 'c) result Lwt.t -> ('b, 'c) result Lwt.t 
   end
   end

   module ComposeM (M : MonadM)(N : MonadM) : MonadM  *)