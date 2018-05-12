(** This module define a functor which in order to be defined as such
    should satisfy the following two laws:
  - fmap id = id
  - fmap (g . f) = (fmap g) . fmap f
*)
module Functor : sig
  module type S = sig
    type 'a f

    val fmap : ('a -> 'b) -> 'a f -> 'b f

    module Infix : sig
      val (<$) : 'a -> 'b f -> 'a f
    end
  end
end
