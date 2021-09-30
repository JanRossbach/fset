MACHINE fun_extract
SETS
  A = {VAR1, VAR2}
VARIABLES
  p
INVARIANT
  p : A --> 5..6
INITIALISATION
  p := {VAR1 |-> 5, VAR2 |-> 6}
END
