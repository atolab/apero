module Uuid = struct
  type t = Uuidm.t 

  let state = Random.State.make_self_init ()

  let make = Uuidm.v4_gen state 

  (* Note: ns_apero namespace is inspired by those specified in https://tools.ietf.org/html/rfc4122#appendix-C *)
  let ns_apero = Uuidm.of_string "6ba7b842-9dad-11d1-80b4-00c04fd430c8" |> Common.Option.get
  let make_from_string = Uuidm.v5 ns_apero

  let compare = Uuidm.compare
  let equal = Uuidm.equal

  let of_bytes = Uuidm.of_bytes
  let to_bytes = Uuidm.to_bytes

  let of_string = Uuidm.of_string
  let to_string = Uuidm.to_string

end 
