open Import

type t =
  { obj_dir : Path.Build.t Obj_dir.t
  ; modules : Module.t list
  ; top_sorted_modules : Module.t list Action_builder.t
  ; ext_obj : string
  ; excluded_modules : Module_name.Set.t
  }

let filter_excluded_modules t modules =
  List.partition modules ~f:(fun module_ ->
    let name = Module.name module_ in
    Module_name.Set.mem t.excluded_modules name)
;;

let make ?(excluded_modules = []) ~obj_dir ~modules ~top_sorted_modules ~ext_obj () =
  let modules = Modules.With_vlib.impl_only modules in
  let excluded_modules = Module_name.Set.of_list excluded_modules in
  { obj_dir; modules; top_sorted_modules; ext_obj; excluded_modules }
;;

let cm_files t ~kind modules =
  Obj_dir.Module.L.cm_files t.obj_dir modules ~kind:(Ocaml kind)
;;

let objects_and_cms t ~mode modules =
  let kind = Mode.cm_kind mode in
  let excluded_modules, modules = filter_excluded_modules t modules in
  let excluded_cm_files = cm_files t ~kind excluded_modules in
  let cm_files = cm_files t ~kind modules in
  ( excluded_cm_files
  , match mode with
    | Byte -> cm_files
    | Native ->
      Obj_dir.Module.L.o_files t.obj_dir modules ~ext_obj:t.ext_obj
      |> List.rev_append cm_files )
;;

let unsorted_objects_and_cms t ~mode = objects_and_cms t ~mode t.modules

let top_sorted_cms t ~mode =
  let kind = Mode.cm_kind mode in
  Action_builder.map t.top_sorted_modules ~f:(fun modules ->
    let excluded_modules, modules = filter_excluded_modules t modules in
    cm_files t ~kind excluded_modules, cm_files t ~kind modules)
;;

let top_sorted_objects_and_cms t ~mode =
  Action_builder.map t.top_sorted_modules ~f:(fun modules ->
    objects_and_cms t ~mode modules)
;;
