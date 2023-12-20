Check that we get compile errors when trying to use a read-only or write-only property with the opposite operation:

  $ cat >t.ml <<'EOF'
  > open Xapi_fdcaps.Properties
  > let _ = as_readable (make `wronly `reg) in ()
  > EOF
  $ ocamlfind ocamlc -package xapi-fdcaps -c t.ml
  File "t.ml", line 2, characters 40-42:
  2 | let _ = as_readable (make `wronly `reg) in ()
                                              ^^
  Error: Syntax error
  [2]

  $ cat >t.ml <<'EOF'
  > open Xapi_fdcaps.Properties
  > let _ = as_writable (make `rdonly `reg) in ()
  > EOF
  $ ocamlfind ocamlc -package xapi-fdcaps -c t.ml
  File "t.ml", line 2, characters 40-42:
  2 | let _ = as_writable (make `rdonly `reg) in ()
                                              ^^
  Error: Syntax error
  [2]
