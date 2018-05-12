module Result = struct
  module type S = sig
    type e
    exception InvalidResult
    include  Amonad.Monad with type 'a m = ('a, e) result
    val ok : 'a -> 'a m
    val fail : e -> 'a m
    val get : 'a m -> 'a
    val or_else : 'a m -> (e -> 'a m) -> 'a m
    val (>>|) : 'a m -> (e -> 'a m) -> 'a m
    val is_ok : 'a m -> bool
    val is_error : 'a m -> bool
  end

  module Converter (RB : S) (RA : S) = struct
    let lift_e r f = match r with
      | RA.(Ok v) -> RB.ok v
      | RA.(Error e) -> RB.fail (f e)
    module Infix = struct
      let  (=>>) = lift_e
    end
  end

  module Make (E: sig type e end) = struct
    type e = E.e
    exception InvalidResult

    include Amonad.Make(struct
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
    let get = function
      | Ok a -> a
      | Error _ -> raise InvalidResult

    let  or_else r f = match r with
      | Ok a  -> return a
      | Error e -> f e

    let (>>|) = or_else

    let is_ok = function  | Ok _ -> true | _ -> false

    let is_error r = not @@ is_ok r

  end
end
