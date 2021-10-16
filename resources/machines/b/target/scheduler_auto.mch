MACHINE scheduler
VARIABLES activePID3, activePID1, activePID2, readyPID3, readyPID1, readyPID2, waitingPID3, waitingPID1, waitingPID2
SETS PID={PID1,PID2,PID3}
INVARIANT activePID3:BOOL & activePID1:BOOL & activePID2:BOOL & (readyPID3:BOOL & readyPID1:BOOL & readyPID2:BOOL) & (waitingPID3:BOOL & waitingPID1:BOOL & waitingPID2:BOOL) & (activePID3:BOOL & activePID1:BOOL & activePID2:BOOL) & (readyPID3:BOOL & readyPID1:BOOL & readyPID2:BOOL) & (waitingPID3:BOOL & waitingPID1:BOOL & waitingPID2:BOOL) & card(active)<=1 & (active={} => ready={})
INITIALISATION active := {} || ready := {} || waiting := {}
OPERATIONS
rr <-- nr_readyPID3 = rr := card(ready);
rr <-- nr_readyPID1 = rr := card(ready);
rr <-- nr_readyPID2 = rr := card(ready);
newPID3 = SELECT pp:PID & pp/:active & pp/:ready\/waiting THEN waiting := waiting\/{pp} END ;
newPID1 = SELECT pp:PID & pp/:active & pp/:ready\/waiting THEN waiting := waiting\/{pp} END ;
newPID2 = SELECT pp:PID & pp/:active & pp/:ready\/waiting THEN waiting := waiting\/{pp} END ;
delPID3 = SELECT pp:waiting THEN waiting := waiting-{pp} END ;
delPID1 = SELECT pp:waiting THEN waiting := waiting-{pp} END ;
delPID2 = SELECT pp:waiting THEN waiting := waiting-{pp} END ;
readyPID3 = SELECT rr:waiting THEN waiting := waiting-{rr} || IF active={} THEN active := {rr} ELSE ready := ready\/{rr} END  END ;
readyPID1 = SELECT rr:waiting THEN waiting := waiting-{rr} || IF active={} THEN active := {rr} ELSE ready := ready\/{rr} END  END ;
readyPID2 = SELECT rr:waiting THEN waiting := waiting-{rr} || IF active={} THEN active := {rr} ELSE ready := ready\/{rr} END  END ;
swapPID3 = SELECT active/={} THEN waiting := waiting\/active || IF ready={} THEN active := {} ELSE ANY pp WHERE pp:ready THEN active := {pp} || ready := ready-{pp} END  END  END ;
swapPID1 = SELECT active/={} THEN waiting := waiting\/active || IF ready={} THEN active := {} ELSE ANY pp WHERE pp:ready THEN active := {pp} || ready := ready-{pp} END  END  END ;
swapPID2 = SELECT active/={} THEN waiting := waiting\/active || IF ready={} THEN active := {} ELSE ANY pp WHERE pp:ready THEN active := {pp} || ready := ready-{pp} END  END  END 
END