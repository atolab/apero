module Uuid = struct
  type t = Uuidm.t 
  
  let state = Random.State.make_self_init ()

  let make = Uuidm.v4_gen state 
    
  let next_id = make 
  
  let compare = Uuidm.compare
  let equal = Uuidm.equal
  
  let of_bytes = Uuidm.of_bytes
  let to_bytes = Uuidm.to_bytes
  
  let of_string = Uuidm.of_string
  let to_string = Uuidm.to_string
  
end 
