module type S = sig
  type t

  val empty : t
  val zero : unit -> t

  val append : t  -> t -> t
  val plus : t -> t -> t

  val concat : t list -> t
  (** [concat ms] is the the same as fold_right append empty*)

  module  Infix : sig
    val (<>) : t -> t -> t
    (** [(<>)] is the infix version of {e append} *)
  end
end


module Make  (M: sig type t val empty: t val append : t -> t -> t end) : (S with type t = M.t)

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

module MakeP  (M: sig type 'a t val empty: 'a t val append : 'a t -> 'a t -> 'a t end) : (Sp with type 'a t = 'a M.t)
