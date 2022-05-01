open! Dune_engine
open! Stdune
open Import
module SC = Super_context

module Includes = struct
  type t = { entry_modules : Lib.t Module_name.Map.t Resolve.Memo.t }

  let args ~project ~opaque ~requires_compile ~requires ~cm_kind =
    let iflags libs mode = Lib.L.include_flags ~project libs mode in
    match cm_kind with
    | Cm_kind.Cmo | Cmi ->
      Command.Args.memo
        (Command.Args.S
           [ iflags requires_compile Byte
           ; Hidden_deps (Lib_file_deps.deps requires ~groups:[ Cmi ])
           ])
    | Cmx ->
      Command.Args.memo
        (Command.Args.S
           [ iflags requires_compile Native
           ; Hidden_deps
               (if opaque then
                List.map requires ~f:(fun lib ->
                    ( lib
                    , if Lib.is_local lib then [ Lib_file_deps.Group.Cmi ]
                      else [ Cmi; Cmx ] ))
                |> Lib_file_deps.deps_with_exts
               else
                 Lib_file_deps.deps requires
                   ~groups:[ Lib_file_deps.Group.Cmi; Cmx ])
           ])

  let make ~requires : t =
    let entry_modules =
      let open Resolve.Memo.O in
      let* requires = requires in
      let f acc lib =
        let+ entry_modules = Lib.entry_module_names lib in
        let f acc entry_module =
          match Module_name.Map.add acc entry_module lib with
          | Ok acc -> acc
          | Error _ -> acc
        in
        List.fold_left entry_modules ~f ~init:acc
      in
      Resolve.Memo.List.fold_left requires ~f ~init:Module_name.Map.empty
    in
    { entry_modules }

  let empty = { entry_modules = Resolve.Memo.return Module_name.Map.empty }
end

type opaque =
  | Explicit of bool
  | Inherit_from_settings

let eval_opaque (context : Context.t) = function
  | Explicit b -> b
  | Inherit_from_settings ->
    Profile.is_dev context.profile
    && Ocaml_version.supports_opaque_for_mli context.version

type modules =
  { modules : Modules.t
  ; dep_graphs : Dep_graph.t Ml_kind.Dict.t
  }

let singleton_modules m =
  { modules = Modules.singleton m; dep_graphs = Dep_graph.Ml_kind.dummy m }

type t =
  { super_context : Super_context.t
  ; scope : Scope.t
  ; expander : Expander.t
  ; obj_dir : Path.Build.t Obj_dir.t
  ; modules : modules
  ; flags : Ocaml_flags.t
  ; requires_compile : Lib.t list Resolve.Memo.t
  ; requires_link : Lib.t list Resolve.t Memo.Lazy.t
  ; includes : Includes.t
  ; preprocessing : Pp_spec.t
  ; opaque : bool
  ; stdlib : Ocaml_stdlib.t option
  ; js_of_ocaml : Js_of_ocaml.In_context.t option
  ; sandbox : Sandbox_config.t
  ; package : Package.t option
  ; vimpl : Vimpl.t option
  ; modes : Mode.Dict.Set.t
  ; bin_annot : bool
  ; ocamldep_modules_data : Ocamldep.Modules_data.t
  }

let super_context t = t.super_context

let scope t = t.scope

let expander t = t.expander

let dir t = Obj_dir.dir t.obj_dir

let obj_dir t = t.obj_dir

let modules t = t.modules.modules

let flags t = t.flags

let requires_compile t = t.requires_compile

let requires_link t = Memo.Lazy.force t.requires_link

let includes t ~cm_kind m =
  let open Action_builder.O in
  let+ requires_compile = Resolve.Memo.read t.requires_compile
  and+ requires =
    if Module.is_generated m then Resolve.Memo.read t.requires_compile
    else
      let ml_kind = Cm_kind.source cm_kind in
      let+ deps =
        Ocamldep.raw_read_immediate_deps_of ~obj_dir:t.obj_dir ~ml_kind m
      and+ entry_modules = Resolve.Memo.read t.includes.entry_modules in
      let f acc dep =
        match
          Module_name.Map.find entry_modules (Module_name.of_string dep)
        with
        | Some lib -> Lib.Set.add acc lib
        | None -> acc
      in
      Lib.Set.to_list (List.fold_left deps ~f ~init:Lib.Set.empty)
  in
  let project = Scope.project t.scope in
  Includes.args ~project ~opaque:t.opaque ~requires_compile ~requires ~cm_kind

let preprocessing t = t.preprocessing

let opaque t = t.opaque

let stdlib t = t.stdlib

let js_of_ocaml t = t.js_of_ocaml

let sandbox t = t.sandbox

let set_sandbox t sandbox = { t with sandbox }

let package t = t.package

let vimpl t = t.vimpl

let modes t = t.modes

let bin_annot t = t.bin_annot

let context t = Super_context.context t.super_context

let ocamldep_modules_data t = t.ocamldep_modules_data

let dep_graphs t = t.modules.dep_graphs

let create ~super_context ~scope ~expander ~obj_dir ~modules ~flags
    ~requires_compile ~requires_link ?(preprocessing = Pp_spec.dummy) ~opaque
    ?stdlib ~js_of_ocaml ~package ?vimpl ?modes ?(bin_annot = true) () =
  let open Memo.O in
  let project = Scope.project scope in
  let requires_compile =
    if Dune_project.implicit_transitive_deps project then
      Memo.Lazy.force requires_link
    else requires_compile
  in
  let sandbox =
    (* With sandboxing, there are a few build errors in ocaml platform 1162238ae
       like: File "ocaml_modules/ocamlgraph/src/pack.ml", line 1: Error: The
       implementation ocaml_modules/ocamlgraph/src/pack.ml does not match the
       interface
       ocaml_modules/ocamlgraph/src/.graph.objs/byte/graph__Pack.cmi: *)
    Sandbox_config.no_sandboxing
  in
  let modes =
    let default =
      Mode.Dict.make_both (Some Dune_file.Mode_conf.Kind.Inherited)
    in
    Option.value ~default modes |> Mode.Dict.map ~f:Option.is_some
  in
  let opaque = eval_opaque (Super_context.context super_context) opaque in
  let ocamldep_modules_data : Ocamldep.Modules_data.t =
    { dir = Obj_dir.dir obj_dir
    ; obj_dir
    ; sctx = super_context
    ; vimpl
    ; modules
    ; stdlib
    }
  in
  let+ dep_graphs = Dep_rules.rules ocamldep_modules_data in
  { super_context
  ; scope
  ; expander
  ; obj_dir
  ; modules = { modules; dep_graphs }
  ; flags
  ; requires_compile
  ; requires_link
  ; includes = Includes.make ~requires:requires_compile
  ; preprocessing
  ; opaque
  ; stdlib
  ; js_of_ocaml
  ; sandbox
  ; package
  ; vimpl
  ; modes
  ; bin_annot
  ; ocamldep_modules_data
  }

let for_alias_module t alias_module =
  let flags =
    let project = Scope.project t.scope in
    let dune_version = Dune_project.dune_version project in
    let profile = (Super_context.context t.super_context).profile in
    Ocaml_flags.default ~dune_version ~profile
  in
  let sandbox =
    let ctx = Super_context.context t.super_context in
    (* If the compiler reads the cmi for module alias even with [-w -49
       -no-alias-deps], we must sandbox the build of the alias module since the
       modules it references are built after. *)
    if Ocaml_version.always_reads_alias_cmi ctx.version then
      Sandbox_config.needs_sandboxing
    else Sandbox_config.no_special_requirements
  in
  let modules : modules =
    match Modules.is_stdlib_alias t.modules.modules alias_module with
    | false -> singleton_modules alias_module
    | true ->
      (* The stdlib alias module is different from the alias modules usually
         produced by Dune: it contains code and depends on a few other
         [CamlinnternalXXX] modules from the stdlib, so we need the full set of
         modules to compile it. *)
      t.modules
  in
  { t with
    flags =
      Ocaml_flags.append_common flags
        [ "-w"; "-49"; "-nopervasives"; "-nostdlib" ]
  ; includes = Includes.empty
  ; stdlib = None
  ; sandbox
  ; modules
  }

let for_root_module t root_module =
  let flags =
    let project = Scope.project t.scope in
    let dune_version = Dune_project.dune_version project in
    let profile = (Super_context.context t.super_context).profile in
    Ocaml_flags.default ~profile ~dune_version
  in
  { t with
    flags =
      Ocaml_flags.append_common flags
        [ "-w"; "-49"; "-nopervasives"; "-nostdlib" ]
  ; stdlib = None
  ; modules = singleton_modules root_module
  }

let for_module_generated_at_link_time cctx ~requires ~module_ =
  let opaque =
    (* Cmi's of link time generated modules are compiled with -opaque, hence
       their implementation must also be compiled with -opaque *)
    let ctx = Super_context.context cctx.super_context in
    Ocaml_version.supports_opaque_for_mli ctx.version
  in
  let modules = singleton_modules module_ in
  { cctx with
    opaque
  ; flags = Ocaml_flags.empty
  ; requires_link = Memo.lazy_ (fun () -> requires)
  ; requires_compile = requires
  ; modules
  }

let for_wrapped_compat t = { t with includes = Includes.empty; stdlib = None }

let for_plugin_executable t ~embed_in_plugin_libraries =
  let libs = Scope.libs t.scope in
  let requires_link =
    Memo.lazy_ (fun () ->
        Resolve.Memo.List.map ~f:(Lib.DB.resolve libs) embed_in_plugin_libraries)
  in
  { t with requires_link }

let without_bin_annot t = { t with bin_annot = false }

let root_module_entries t =
  let open Action_builder.O in
  let* requires = Resolve.Memo.read t.requires_compile in
  let* l =
    Action_builder.List.map requires ~f:(fun lib ->
        Action_builder.of_memo (Lib.entry_module_names lib) >>= Resolve.read)
  in
  Action_builder.return (List.concat l)
