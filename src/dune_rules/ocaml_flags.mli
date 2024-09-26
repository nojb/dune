(** OCaml flags *)

open! Import

type t

module Spec : sig
  type t

  val equal : t -> t -> bool
  val decode : t Dune_lang.Decoder.fields_parser
  val standard : t

  val make
    :  common:Ordered_set_lang.Unexpanded.t
    -> specific:Ordered_set_lang.Unexpanded.t Lib_mode.Map.t
    -> t

  module Per_module : sig
    type flags := t
    type t

    val for_all : flags -> t

    val make
      :  common:Ordered_set_lang.Unexpanded.t
      -> specific:Ordered_set_lang.Unexpanded.t Lib_mode.Map.t
      -> t

    val decode : t Dune_lang.Decoder.fields_parser
  end
end

val make
  :  spec:Spec.t
  -> default:t
  -> eval:
       (Ordered_set_lang.Unexpanded.t
        -> standard:string list Action_builder.t
        -> string list Action_builder.t)
  -> t

val default : dune_version:Dune_lang.Syntax.Version.t -> profile:Profile.t -> t
val empty : t
val get : t -> Lib_mode.t -> string list Action_builder.t
val dump : t -> Dune_lang.t list Action_builder.t
val open_flags : Module_name.t list -> string list

module Per_module : sig
  type flags := t
  type t

  val for_all : flags -> t
  val empty : t
  val of_list : string list -> t
  val allow_only_melange : t -> t
  val default : dune_version:Dune_lang.Syntax.Version.t -> profile:Profile.t -> t
  val get : t -> Module_name.t -> Lib_mode.t -> string list Action_builder.t
  val get_default : t -> Lib_mode.t -> string list Action_builder.t
  val append_common : t -> string list -> t
  val with_vendored_flags : t -> ocaml_version:Version.t -> t

  val make
    :  spec:Spec.Per_module.t
    -> default:flags
    -> eval:
         (Ordered_set_lang.Unexpanded.t
          -> standard:string list Action_builder.t
          -> string list Action_builder.t)
    -> t
end
