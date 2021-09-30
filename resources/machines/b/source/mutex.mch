MACHINE Mutex
SETS Status = {noncrit, wait, crit}
CONSTANTS n
PROPERTIES n=2
VARIABLES pc, y
INVARIANT pc: 1..n --> Status & y : 0..1 & card({i|i|->crit : pc}) <= 1
INITIALISATION pc := (1..n)*{noncrit} || y := 1
OPERATIONS
    RequestCS(i) = PRE i:1..n & pc(i)=noncrit THEN pc(i) := wait END;
    EnterCS(i) = PRE i:1..n & pc(i)=wait & y>0 THEN pc(i) := crit || y:=y-1 END;
    LeaveCS(i) = PRE i:1..n & pc(i)=crit THEN pc(i) := noncrit || y:=y+1 END
END
