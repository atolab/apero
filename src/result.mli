
module type S = sig
  type e
  include  Monad.Monad with type 'a m = ('a, e) result
  val ok : 'a -> 'a m
  val fail : e -> 'a m
end

module Converter (RA : S) (RB : S) : sig
  val  lift_e : 'a RA.m -> (RA.e -> RB.e) -> 'a RB.m
end

module Make (E : sig type e end ) : S with type e = E.e
