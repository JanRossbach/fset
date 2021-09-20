MACHINE scheduler
SETS PID
VARIABLES active, ready, waiting
INVARIANT active:POW(PID) & ready:POW(PID) & waiting:POW(PID) & active<:PID & ready<:PID & waiting<:PID & ready/\waiting={} & active/\(ready\/waiting)={} & card(active)<=1 & (active={} => ready={})
INITIALISATION active,ready,waiting := {},{},{}
OPERATIONS
rr <-- nr_ready = rr := card(ready);
new(pp) = SELECT pp:PID & pp/:active & pp/:ready\/waiting THEN waiting := waiting\/{pp} END ;
del(pp) = SELECT pp:waiting THEN waiting := waiting\{pp} END ;
ready(rr) = SELECT rr:waiting THEN waiting := waiting\{rr} ELSE IF {}=active THEN active := active\/{rr} ELSE ready := ready\/{rr} END  END ;
swap = SELECT active/={}
     THEN waiting := waiting\/active
     ELSE IF ready={}
     THEN active := {} ELSE ANY pp WHERE pp:ready THEN active,ready := {pp},ready\{pp} END  END  END
END