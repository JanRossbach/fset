MACHINE b2sat_test
SETS S = {s1,s2}
VARIABLES
  s, x
INVARIANT
  s <: S &
  x : S
INITIALISATION
  x :: S ||
  s := {}
OPERATIONS
  add_x = SELECT x/: s THEN s := s \/ {x} END
END