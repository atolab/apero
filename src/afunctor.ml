module Functor = struct
  module type S = sig
    type 'a f

    val fmap : ('a -> 'b) -> 'a f -> 'b f

    module Infix : sig
      val (<$) : 'a -> 'b f -> 'a f
    end
  end
end
