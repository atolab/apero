open Ordered

module Property = struct  
  
  module type S = sig 
 
    module Key : Ordered.S      
    module Value : Ordered.S      
    
    module Map : (module type of Map.Make(Key))

    include Ordered.S with type t = Key.t * Value.t
 
    val make : Key.t -> Value.t -> t 
    val key : t -> Key.t 
    val value : t -> Value.t
    
  end

  module Make 
    (K : Comparable)
    (V : Comparable) = struct 

    module Key = Ordered.Make(K)
    module Value = Ordered.Make(V)

    module Map = Map.Make(Key)    

    module C = struct 
      type t = Key.t * Value.t
      let compare (k1,v1) (k2,v2) = match (Key.compare k1 k2, Value.compare v1 v2) with 
      | (0, 0) -> 0
      | (a, _) -> a 
    end
    
    include Ordered.Make (C) 

    let make k v = (k, v)
    let key (k, _) = k
    let value (_, v) = v    
    
  end
end

(* open Atypes

open Iobuf

module Property = struct
  (* type id_t = Vle.t *)
  type t = Vle.t * IOBuf.t
  let create id data = (id, data)
  let id p = fst p
  let data p = snd p

end

module Properties = struct
  type t = Property.t list
  let empty = []
  let singleton p = [p]
  let add p ps = p::ps
  let find f ps = List.find_opt f ps
  let get name ps = List.find_opt (fun (n, _) -> if n = name then true else false) ps
  let length ps = List.length ps
  let of_list xs = xs
  let to_list ps = ps
end *)
