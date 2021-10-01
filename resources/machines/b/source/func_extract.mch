MACHINE fun_extract
SETS
  A = {VAR1, VAR2}
VARIABLES
  p, q
INVARIANT
  p : A --> 5..6 & q < 15
INITIALISATION
  p := {VAR1 |-> 5, VAR2 |-> 6} || q  :=  7
OPERATIONS
run = q := q + p(VAR1)
END
