open Import

let flag_of_kind : Ml_kind.t -> _ =
  function
  | Impl -> "--impl"
  | Intf -> "--intf"

let add_diff sctx loc alias ~dir ~input ~output =
  let open Build.O in
  let action = Action.diff input output in
  Super_context.add_alias_action sctx alias ~dir ~loc:(Some loc) ~locks:[]
    ~stamp:input
    (Build.paths [input; output]
     >>>
     Build.action
       ~dir:(Path.build dir)
       ~targets:[]
       action)

let rec subdirs_until_root dir =
  match Path.parent dir with
  | None -> [dir]
  | Some d -> dir :: subdirs_until_root d

let depend_on_files ~named dir =
  subdirs_until_root dir
  |> List.concat_map ~f:(fun dir -> List.map named ~f:(Path.relative dir))
  |> Build.paths_existing

let formatted = ".formatted"

let gen_rules_output sctx (config : Dune_file.Auto_format.t) ~expander ~output_dir =
  assert (formatted = Path.Build.basename output_dir);
  let loc = Dune_file.Auto_format.loc config in
  let dir = Path.Build.parent_exn output_dir in
  let source_dir = Path.Build.drop_build_context_exn dir in
  let alias_formatted = Alias.fmt ~dir:output_dir in
  let resolve_program =
    Super_context.resolve_program ~dir sctx ~loc:(Some loc) in
  let ocamlformat_deps = lazy (
    depend_on_files ~named:[".ocamlformat"; ".ocamlformat-ignore"]
      (Path.source source_dir)
  ) in
  let setup_formatting file =
    let input_basename = Path.Source.basename file in
    let input = Path.Build.relative dir input_basename in
    let output = Path.Build.relative output_dir input_basename in

    let ocaml kind =
      if Dune_file.Auto_format.includes config Ocaml then
        let exe = resolve_program "ocamlformat" in
        let args =
          [ Command.Args.A (flag_of_kind kind)
          ; Dep (Path.build input)
          ; A "--name"
          ; Path (Path.source file)
          ; A "-o"
          ; Target output
          ]
        in
        Some (
          Build.S.seq (Build.S.ignore (Lazy.force ocamlformat_deps))
            (Command.run
               ~dir:(Path.build (Super_context.build_dir sctx)) exe args))
      else
        None
    in

    let formatter =
      let input = Path.build input in
      match Path.Source.basename file, Path.Source.extension file with
      | _, ".ml" -> ocaml Impl
      | _, ".mli" -> ocaml Intf
      | _, (".re"|".rei" as ext) when Dune_file.Auto_format.includes config Reason ->
        begin match Dialect.format Dialect.reason (if ext = ".re" then Impl else Intf) with
        | Dialect.Filter.No_filter ->
          assert false
        | Action (loc, action) ->
          let src = Path.as_in_build_dir_exn input in
          Some (Preprocessing.action_for_pp sctx ~dep_kind:Lib_deps_info.Kind.Required
                  ~loc ~expander ~action ~src ~target:(Some output))
        end
      | "dune", _ when Dune_file.Auto_format.includes config Dune ->
        let exe = resolve_program "dune" in
        let args = [Command.Args.A "format-dune-file"; Dep input] in
        let dir = Path.build dir in
        Some (Command.run ~dir ~stdout_to:output exe args)
      | _ -> None
    in

    Option.iter formatter ~f:(fun arr ->
      Super_context.add_rule sctx ~mode:Standard ~loc ~dir arr;
      add_diff sctx loc alias_formatted ~dir
        ~input:(Path.build input) ~output:(Path.build output))
  in
  File_tree.files_of (Super_context.file_tree sctx) source_dir
  |> Path.Source.Set.iter ~f:setup_formatting;
  Rules.Produce.Alias.add_deps alias_formatted Path.Set.empty

let gen_rules ~dir =
  let output_dir = Path.Build.relative dir formatted in
  let alias = Alias.fmt ~dir in
  let alias_formatted = Alias.fmt ~dir:output_dir in
  Alias.stamp_file alias_formatted
  |> Path.build
  |> Path.Set.singleton
  |> Rules.Produce.Alias.add_deps alias
