module Result : sig
  module type S = sig
    type e
    exception InvalidResult
    include  Amonad.Monad with type 'a m = ('a, e) result
    val ok : 'a -> 'a m
    val fail : e -> 'a m
    val get : 'a m -> ' a
    val or_else : 'a m -> (e -> 'a m) -> 'a m
    val (>>|) : 'a m -> (e -> 'a m) -> 'a m
    val is_ok : 'a m -> bool
    val is_error : 'a m -> bool
  end

  module Converter (RB : S) (RA : S) : sig
    val  lift_e : 'a RA.m -> (RA.e -> RB.e) -> 'a RB.m
    module Infix  : sig
      val (=>>) : 'a RA.m -> (RA.e -> RB.e) -> 'a RB.m
    end
  end

  module Make (E : sig type e end ) : S with type e = E.e
end
