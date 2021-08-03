
  $ dune build @lint -p testsuite1 --force
      mylinter alias lint
  File "lib.ml", line 4, characters 0-18:
  4 | type myAst = MyAst
      ^^^^^^^^^^^^^^^^^^
  Alert zanuda-linter: Type name `myAst` should be in snake case
  $ dune build testlib2.cmxa
  $ dune build @lint -p testsuite2 --force
      mylinter alias lint
  File "Parsetree.mli", lines 1-4, characters 0-17:
  1 | type exprA =
  2 |   | App of exprA * exprA
  3 |   | Abs of string * exprA
  4 |   | Var of string
  Alert zanuda-linter: Type name `exprA` should be in snake case
  rdjson reports about exprA
  Golint reports about exprA
  File "Parsetree.mli", line 2, characters 2-24:
  2 |   | App of exprA * exprA
        ^^^^^^^^^^^^^^^^^^^^^^
  Alert zanuda-linter: Constructor 'App' has no documentation attribute
  File "Parsetree.mli", line 3, characters 2-25:
  3 |   | Abs of string * exprA
        ^^^^^^^^^^^^^^^^^^^^^^^
  Alert zanuda-linter: Constructor 'Abs' has no documentation attribute
