MACHINE scheduler
SETS PID={PID1,PID2,PID3}
VARIABLES active, ready, waiting
INVARIANT active:POW(PID) & ready:POW(PID) & waiting:POW(PID) & active<:PID & ready<:PID & waiting<:PID & ready/\waiting={} & active/\(ready\/waiting)={} & card(active)<=1 & (active={} => ready={})
INITIALISATION active := {} || ready := {} || waiting := {}
OPERATIONS
rr <-- nr_ready = rr := card(ready);
newPID1 = SELECT PID1/:active & PID1/:ready\/waiting THEN waiting := waiting\/{PID1} END ;
newPID2 = SELECT PID2/:active & PID2/:ready\/waiting THEN waiting := waiting\/{PID2} END ;
newPID3 = SELECT PID3/:active & PID3/:ready\/waiting THEN waiting := waiting\/{PID3} END ;
delPID1 = SELECT PID1:waiting THEN waiting := waiting-{PID1} END ;
delPID2 = SELECT PID2:waiting THEN waiting := waiting-{PID2} END ;
delPID3 = SELECT PID3:waiting THEN waiting := waiting-{PID3} END ;
readyPID1 = SELECT PID1:waiting THEN waiting := waiting-{PID1} || IF not(active=TRUE) THEN active := {PID1} ELSE ready := ready\/{PID1} END  END ;
readyPID2 = SELECT PID2:waiting THEN waiting := waiting-{PID2} || IF not(active=TRUE) THEN active := {PID2} ELSE ready := ready\/{PID2} END  END ;
readyPID3 = SELECT PID3:waiting THEN waiting := waiting-{PID3} || IF not(active=TRUE) THEN active := {PID3} ELSE ready := ready\/{PID3} END  END ;
swapPID1 = SELECT PID1:ready & active=TRUE THEN waiting := waiting\/active || IF not(ready=TRUE) THEN active := {} ELSE active := {PID1} || ready := ready-{PID1} END  END ;
swapPID2 = SELECT PID2:ready & active=TRUE THEN waiting := waiting\/active || IF not(ready=TRUE) THEN active := {} ELSE active := {PID2} || ready := ready-{PID2} END  END ;
swapPID3 = SELECT PID3:ready & active=TRUE THEN waiting := waiting\/active || IF not(ready=TRUE) THEN active := {} ELSE active := {PID3} || ready := ready-{PID3} END  END 
END