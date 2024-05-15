Test if the issue is solved
  $ ver=$(ocamlc -H x 2>&1)
  > case "$ver" in
  > "No input files")
  >   dune build
  >  ;;
  > esac
