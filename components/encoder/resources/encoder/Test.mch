MACHINE Test
SETS
 A={aa,bb};
 B = {cc,dd}
VARIABLES a, y
INVARIANT a:POW(A) & y:A & y=aa<=>a={}
INITIALISATION y:=aa || a:={x|x:A & x=aa}
OPERATIONS
hello(x) = SELECT x:A THEN y:=x || a:=a\/{z|z:A & z=y} END
END