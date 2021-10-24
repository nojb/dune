open Stdune

module Arch : sig
  type t =
    | X86
    | X64

  val to_dyn : t -> Dyn.t

  val equal : t -> t -> bool

  val hash : t -> int
end

val detect : Arch.t -> Env.t -> Env.t Memo.Build.t
