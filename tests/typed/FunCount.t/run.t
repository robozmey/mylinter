  $ dune build
  $ mylinter -dir .
  File "FunCount.ml", line 39, characters 0-41:
  39 | let anime : int -> int = (fun x -> x + 1)
       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Alert zanuda-linter: This file contains 105 but less 100 expected!
  
