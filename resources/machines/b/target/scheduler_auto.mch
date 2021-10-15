MACHINE scheduler
VARIABLES activePID0, activePID1, activePID2, readyPID0, readyPID1, readyPID2, waitingPID0, waitingPID1, waitingPID2
SETS PID={PID1,PID2,PID3}
INVARIANT activePID0:BOOL & activePID1:BOOL & activePID2:BOOL & (readyPID0:BOOL & readyPID1:BOOL & readyPID2:BOOL) & (waitingPID0:BOOL & waitingPID1:BOOL & waitingPID2:BOOL) & card(active)<=1 & (active={} => ready={})
INITIALISATION active := {} || ready := {} || waiting := {}
OPERATIONS
rr <-- nr_ready = rr := card(ready);
new(pp) = SELECT pp:PID & pp/:active & pp/:ready\/waiting THEN waiting := waiting\/{pp} END ;
del(pp) = SELECT pp:waiting THEN waiting := waiting-{pp} END ;
ready(rr) = SELECT rr:waiting THEN waiting := waiting-{rr} || IF active={} THEN active := {rr} ELSE ready := ready\/{rr} END  END ;
swap = SELECT active/={} THEN waiting := waiting\/active || IF ready={} THEN active := {} ELSE ANY pp WHERE pp:ready THEN active := {pp} || ready := ready-{pp} END  END  END 
END