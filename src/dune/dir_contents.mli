(** Directories contents *)

(** This modules takes care of attaching modules and mlds files found in the
  source tree or generated by user rules to library, executables, tests and
    documentation stanzas. *)

open! Stdune
open Import

module Dir_artifacts : sig
  type t

  val lookup_module :
    t -> Module_name.t -> Path.Build.t Obj_dir.Module.Single.t option

  val lookup_library : t -> Lib_name.t -> Dune_file.Library.t option
end

type t

val dir : t -> Path.Build.t

(** Files in this directory. At the moment, this doesn't include all generated
  files, just the ones generated by [rule], [ocamllex], [ocamlyacc], [menhir]
    stanzas. *)
val text_files : t -> String.Set.t

(** Modules attached to a library. [name] is the library best name. *)
val modules_of_library : t -> name:Lib_name.t -> Modules.t

val c_sources_of_library : t -> name:Lib_name.t -> C.Sources.t

(** Modules attached to a set of executables. *)
val modules_of_executables :
  t -> obj_dir:Path.Build.t Obj_dir.t -> first_exe:string -> Modules.t

(** Find out what buildable a module is part of *)
val lookup_module : t -> Module_name.t -> Dune_file.Buildable.t option

(** Artifacts defined in this directory *)
val artifacts : t -> Dir_artifacts.t

(** All mld files attached to this documentation stanza *)
val mlds : t -> Dune_file.Documentation.t -> Path.Build.t list

(** Coq modules of library [name] is the Coq library name. *)
val coq_modules_of_library : t -> name:Lib_name.t -> Coq_module.t list

(** Get the directory contents of the given directory. *)
val get : Super_context.t -> dir:Path.Build.t -> t

(** All directories in this group if [t] is a group root or just [t] if it is
  not part of a group. *)
val dirs : t -> t list

type gen_rules_result =
  | Standalone_or_root of t * t list  (** Sub-directories part of the group *)
  | Group_part of Path.Build.t

(** In order to compute the directory contents, we need to interpret stanzas
  such as [rule] or [copy_files]. For such stanzas, computing the targets is
    very similar to interpreting the stanza and compiling it down to low-level
    rules.

    As a result, we proceed as follow: we interpret the stanza into rules and
    extract the targets of the computed rule. This function simply emits these
    rules so that they can be collected by [Build_system].

    However, if the directory is part of a group, this function simply returns
    the root of the group without emitting any rule. *)
val gen_rules : Super_context.t -> dir:Path.Build.t -> gen_rules_result
