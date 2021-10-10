let path = ref (Sys.getcwd ())

let () =
  Arg.parse [] (fun s -> path := s) ""

let path = !path

let rdcw =
  Rdcw.create ~path ~f:(fun _ ev -> print_endline (Dyn.to_string (Rdcw.Event.to_dyn ev)))

let () =
  Rdcw.loop rdcw
