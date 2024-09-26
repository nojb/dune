open Import

(** Compute the ocaml flags based on the directory environment and a buildable
    stanza *)
val ocaml_flags
  :  Super_context.t
  -> dir:Path.Build.t
  -> Ocaml_flags.Spec.Per_module.t
  -> Ocaml_flags.Per_module.t Memo.t

val link_flags
  :  Super_context.t
  -> dir:Path.Build.t
  -> Link_flags.Spec.t
  -> Link_flags.t Memo.t

val link_env : dir:Path.Build.t -> Link_flags.t Memo.t
val ocaml_flags_env : dir:Path.Build.t -> Ocaml_flags.t Memo.t
