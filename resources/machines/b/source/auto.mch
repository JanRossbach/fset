MACHINE scheduler
SETS PID={PID1,PID2,PID3}
VARIABLES active, ready, waiting
INVARIANT active:POW(PID) & ready:POW(PID) & waiting:POW(PID) &
	   (not(PID1:ready & PID1:waiting) & not(PID2:ready & PID2:waiting) & not(PID3:ready & PID3:waiting)) &
	   (not(PID1:active & (PID1:ready or PID1:waiting)) & not(PID2:active & (PID2:ready or PID2:waiting)) & not(PID3:active & (PID3:ready or PID3:waiting))) &
                   (PID1/:active & PID2/:active & PID3/:active => PID1/:ready & PID2/:ready & PID3/:ready)
INITIALISATION active := {} || ready := {} || waiting := {}
OPERATIONS
rr <-- nr_ready = rr := card(ready);
new(pp) = SELECT pp:PID & pp/:active & pp/:ready\/waiting THEN waiting := waiting\/{pp} END ;
del(pp) = SELECT pp:waiting THEN waiting := waiting-{pp} END ;
ready(rr) = SELECT rr:waiting THEN waiting := waiting-{rr} || IF active={} THEN active := {rr} ELSE ready := ready\/{rr} END  END ;
swap = SELECT active/={} THEN waiting := waiting\/active || IF ready={} THEN active := {} ELSE ANY pp WHERE pp:ready THEN active := {pp} || ready := ready-{pp} END  END  END 
END