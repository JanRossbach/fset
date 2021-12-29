MACHINE Numbers
CONSTANTS z
PROPERTIES z=3
VARIABLES x,y
INVARIANT x:INTEGER & x>0 & x<=10 & y:NAT
INITIALISATION x:=0 || y:=0
OPERATIONS
  inc =
       SELECT x<10
       THEN x:=x+1
  END;
  rr <-- add = rr:=x+y+z
END
