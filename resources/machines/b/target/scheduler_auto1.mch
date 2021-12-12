MACHINE scheduler
SETS PID={PID1,PID2,PID3}
VARIABLES activePID1, activePID2, activePID3, ready, waiting
INVARIANT activePID1:BOOL & activePID2:BOOL & activePID3:BOOL & IF activePID1=TRUE THEN {PID1} ELSE {} END\/IF activePID2=TRUE THEN {PID2} ELSE {} END\/IF activePID3=TRUE THEN {PID3} ELSE {} END:POW(PID) & ready:POW(PID) & waiting:POW(PID) & IF activePID1=TRUE THEN {PID1} ELSE {} END\/IF activePID2=TRUE THEN {PID2} ELSE {} END\/IF activePID3=TRUE THEN {PID3} ELSE {} END<:PID & ready<:PID & waiting<:PID & ready/\waiting={} & IF activePID1=TRUE THEN {PID1} ELSE {} END\/IF activePID2=TRUE THEN {PID2} ELSE {} END\/IF activePID3=TRUE THEN {PID3} ELSE {} END/\(ready\/waiting)={} & card(IF activePID1=TRUE THEN {PID1} ELSE {} END\/IF activePID2=TRUE THEN {PID2} ELSE {} END\/IF activePID3=TRUE THEN {PID3} ELSE {} END)<=1 & (IF activePID1=TRUE THEN {PID1} ELSE {} END\/IF activePID2=TRUE THEN {PID2} ELSE {} END\/IF activePID3=TRUE THEN {PID3} ELSE {} END={} => ready={})
INITIALISATION activePID1 := FALSE || activePID2 := FALSE || activePID3 := FALSE || ready := {} || waiting := {}
OPERATIONS
rr <-- nr_ready = rr := card(ready);
newPID1 = SELECT not(activePID1=TRUE) & PID1/:ready\/waiting THEN waiting := waiting\/{PID1} END ;
newPID2 = SELECT not(activePID2=TRUE) & PID2/:ready\/waiting THEN waiting := waiting\/{PID2} END ;
newPID3 = SELECT not(activePID3=TRUE) & PID3/:ready\/waiting THEN waiting := waiting\/{PID3} END ;
delPID1 = SELECT PID1:waiting THEN waiting := waiting-{PID1} END ;
delPID2 = SELECT PID2:waiting THEN waiting := waiting-{PID2} END ;
delPID3 = SELECT PID3:waiting THEN waiting := waiting-{PID3} END ;
readyPID1 = SELECT PID1:waiting THEN waiting := waiting-{PID1} || IF not(activePID1=TRUE) & not(activePID2=TRUE) & not(activePID3=TRUE) THEN activePID1 := TRUE || activePID2 := FALSE || activePID3 := FALSE ELSE ready := ready\/{PID1} END  END ;
readyPID2 = SELECT PID2:waiting THEN waiting := waiting-{PID2} || IF not(activePID1=TRUE) & not(activePID2=TRUE) & not(activePID3=TRUE) THEN activePID1 := FALSE || activePID2 := TRUE || activePID3 := FALSE ELSE ready := ready\/{PID2} END  END ;
readyPID3 = SELECT PID3:waiting THEN waiting := waiting-{PID3} || IF not(activePID1=TRUE) & not(activePID2=TRUE) & not(activePID3=TRUE) THEN activePID1 := FALSE || activePID2 := FALSE || activePID3 := TRUE ELSE ready := ready\/{PID3} END  END ;
swapPID1 = SELECT PID1:ready & not(not(activePID1=TRUE) & not(activePID2=TRUE) & not(activePID3=TRUE)) THEN waiting := waiting\/(IF activePID1=TRUE THEN {PID1} ELSE {} END\/IF activePID2=TRUE THEN {PID2} ELSE {} END\/IF activePID3=TRUE THEN {PID3} ELSE {} END) || IF ready={} THEN activePID1 := FALSE || activePID2 := FALSE || activePID3 := FALSE ELSE activePID1 := TRUE || activePID2 := FALSE || activePID3 := FALSE || ready := ready-{PID1} END  END ;
swapPID2 = SELECT PID2:ready & not(not(activePID1=TRUE) & not(activePID2=TRUE) & not(activePID3=TRUE)) THEN waiting := waiting\/(IF activePID1=TRUE THEN {PID1} ELSE {} END\/IF activePID2=TRUE THEN {PID2} ELSE {} END\/IF activePID3=TRUE THEN {PID3} ELSE {} END) || IF ready={} THEN activePID1 := FALSE || activePID2 := FALSE || activePID3 := FALSE ELSE activePID1 := FALSE || activePID2 := TRUE || activePID3 := FALSE || ready := ready-{PID2} END  END ;
swapPID3 = SELECT PID3:ready & not(not(activePID1=TRUE) & not(activePID2=TRUE) & not(activePID3=TRUE)) THEN waiting := waiting\/(IF activePID1=TRUE THEN {PID1} ELSE {} END\/IF activePID2=TRUE THEN {PID2} ELSE {} END\/IF activePID3=TRUE THEN {PID3} ELSE {} END) || IF ready={} THEN activePID1 := FALSE || activePID2 := FALSE || activePID3 := FALSE ELSE activePID1 := FALSE || activePID2 := FALSE || activePID3 := TRUE || ready := ready-{PID3} END  END 
END