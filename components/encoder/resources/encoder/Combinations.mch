MACHINE Combinations
SETS A = {a1,a2}
CONSTANTS c
PROPERTIES c=1..15
VARIABLES x,y
INVARIANT x:c-->A & y:POW(A)
INITIALISATION x:=c*{a1} || y:={}
OPERATIONS
interaction(z) = SELECT z:1..15 THEN x(z):=a2 || y:=y\/{a2} END
END
