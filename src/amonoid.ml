module Monoid = struct
  module type S = sig
    type t

    val empty : t
    val zero : unit -> t
    val append : t  -> t -> t
    val plus : t -> t -> t
    val concat : t list -> t

    module Infix : sig
      val (<>) : t -> t -> t
    end
  end

  module Make  (M: sig type t val empty: t val append : t -> t -> t end) : (S with type t = M.t) = struct

    type t = M.t

    let empty = M.empty
    let zero () = M.empty
    let append = M.append
    let plus = M.append
    let concat ms = List.fold_right append ms empty

    module Infix = struct
      let  (<>) = append
    end

  end


  module type Sp = sig
    type 'a t

    val empty : 'a t
    val zero : unit -> 'a t
    val append : 'a t  -> 'a t -> 'a t
    val plus : 'a t -> 'a t -> 'a t
    val concat : 'a t list -> 'a t
    (** [concat ms] is the the same as fold_right append empty*)

    module  Infix : sig
      val (<>) : 'a t -> 'a t -> 'a t
      (** [(<>)] is the infix version of {e append} *)
    end
  end

  module MakeP (M: sig type 'a t val empty: 'a t val append : 'a t -> 'a t -> 'a t end) : (Sp with type 'a t = 'a M.t) = struct

    type 'a t = 'a M.t

    let empty = M.empty
    let zero () = M.empty
    let append = M.append
    let plus = M.append
    let concat ms = List.fold_right append ms empty

    module Infix = struct
      let  (<>) = append
    end

  end
end
