
module List : sig

include (module type of List)

  val drop : int -> 'a list -> 'a list
  val take : int -> 'a list -> 'a list
  val zip : 'a list -> 'b list -> ('a * 'b) list
  val unzip : ('a * 'b) list -> ('a list) * ('b list)
end

module ListM : Monad.MonadPlus with type 'a m = 'a list
