
module type S = sig
  type e
  include  Monad.Monad with type 'a m = ('a, e) result
  val ok : 'a -> 'a m
  val fail : e -> 'a m
end

module Converter (RA : S) (RB : S) = struct
  let lift_e r f = match r with
    | RA.(Ok v) -> RB.ok v
    | RA.(Error e) -> RB.fail (f e)
end

module Make (E: sig type e end) = struct
  type e = E.e

  include Monad.Make(struct
      type 'a m = ('a, E.e) result
      let return x = Ok x
      let bind x f = match x with
          Error  e -> Error e
        | Ok x -> f x
      let null x   = match x with
          Error _  -> true
        | _        -> false
    end )

  let ok = return

  let fail e = Error e

end
