module Arch : sig
  type t =
    | X86
    | X64

  val to_dyn : t -> Dyn.t

  val equal : t -> t -> bool

  val hash : t -> int
end

type t =
  { extend_PATH : string
  ; var_LIB : string
  ; var_INCLUDE : string
  }

val to_dyn : t -> Dyn.t

val equal : t -> t -> bool

val hash : t -> int

val detect : Arch.t -> t option Memo.Build.t
