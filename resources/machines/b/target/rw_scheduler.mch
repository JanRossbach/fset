MACHINE scheduler
SETS PID = {PID_1, PID_2, PID3}
VARIABLES active_1, active_2, active_3, ready_PID1, ready_PID2, ready_PID3, waiting
INVARIANT active_1 : BOOL & active_2 : BOOL & active_3 : BOOL & ready_PID1 : BOOL & ready_PID2 : BOOL & ready_PID3 :BOOL &waiting : POW(PID) &
          (ready_PID1=FALSE <=> (PID_1 : )ready/\waiting={} &
          (active_1=FALSE <=> (PID_1:ready\/waiting)) &
          (active_2=FALSE <=> (PID_2:ready\/waiting)) &
          (active_2=FALSE <=> (PID_3:ready\/waiting)) &
          ((active_1=FALSE & active_2=FALSE &active_3=FALSE) or (active_1=TRUE & active_2=FALSE & active_3=FALSE)
          or (active_1=FALSE & active_2=TRUE & active_3=FALSE) or (active_1=FALSE & active_2=FALSE & active_3=TRUE)) &
          ((active_1=FALSE & active_2=FALSE & active_3=FALSE) => ready={})
INITIALISATION active_1:=FALSE || active_2:=FALSE || active_3:=FALSE || ready:={} || waiting:={}
OPERATIONS
rr <-- nr_ready = rr := card(ready);
new(pp) = SELECT pp:PID & pp/:active & pp/:ready\/waiting THEN waiting := waiting\/{pp} END ;
del(pp) = SELECT pp:waiting THEN waiting := waiting-{pp} END ;
ready(rr) = SELECT rr:waiting THEN waiting := waiting-{rr} || IF active={} THEN active := {rr} ELSE ready := ready\/{rr} END  END ;
swap_1 = SELECT acive_1=TRUE THEN waiting := waiting\/PID_1 || IF ready={} THEN active_1:=FALSE ELSE ANY pp WHERE pp:read THEN
