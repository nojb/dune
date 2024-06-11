Should include Foo with -H:
  $ getincludes () {
  >  flag=$1
  >  ITD_flag=$2
  >  # here, we have a compiler that supports -H flag
  >  # case for ITD set to false
  >  echo "(lang dune 3.15)\n(implicit_transitive_deps $ITD_flag)\n" > dune-project
  >  # get the foo includes for run.ml and run.mli
  >  includes=`dune exec --verbose ./run.exe 2>&1 | grep "run.ml" | grep -E -o "\-$flag\s(.foo)\S*" | sed s/\-$flag//g`
  >  includes="$(echo "${includes}" | tr -d '[:space:]')"
  >  echo $includes 
  > }
  > 
  > ocamlc -H x --help > /dev/null
  > fooincludes='.foo.objs/byte.foo.objs/byte.foo.objs/native'
  > if [ $? = 0 ]; then
  >     # here, we have a compiler that supports -H flag
  >     # case for ITD set to false
  >     includes=$(getincludes 'H' 'false')
  >     if [ "$includes" = $fooincludes ]; then
  >       echo "OKAY"
  >     else
  >       echo "ERROR"
  >     fi
  > 
  >     # case for ITD set to true
  >     includes=$(getincludes 'I' 'true')
  >     if [ "$includes" = $fooincludes ]; then
  >       echo "OKAY"
  >     else
  >       echo "ERROR"
  >     fi
  > 
  > else
  >     # here, we have a compiler that does not support -H flag
  >     # case for ITD set to false
  >     includes=$( getincludes 'I' 'false')
  >     if [ "$includes" = '' ]; then
  >       echo "OKAY"
  >     else
  >       echo "ERROR"
  >     fi
  > 
  >     # case for ITD set to true
  >     includes=$( getincludes 'I' 'true')
  >     if [ "$includes" = $fooincludes ]; then
  >       echo "OKAY"
  >     else
  >       echo "ERROR"
  >     fi
  > fi
  OKAY
  OKAY


Test transitive deps can not be directly accessed, both for compiler versions supporting -H or not:
  $ echo "(lang dune 3.15)\n(implicit_transitive_deps false)" > dune-project
  > dune build ./runf.exe 2>&1 | grep -v ocamlc 
  File "runf.ml", line 1, characters 16-21:
  1 | let a = Bar.y + Foo.v
                      ^^^^^
  Error: Unbound module Foo
