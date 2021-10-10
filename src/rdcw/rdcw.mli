module Event : sig
  type t =
    | Buffer_overflow
    | Added of string
    | Removed of string
    | Modified of string
    | Renamed_old_name of string
    | Renamed_new_name of string

  val to_dyn : t -> Dyn.t
end

type t

val create : path:string -> f:(t -> Event.t -> unit) -> t

val loop : t -> unit

val destroy : t -> unit
