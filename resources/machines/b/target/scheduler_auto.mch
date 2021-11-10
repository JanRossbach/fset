MACHINE scheduler
SETS PID={PID1,PID2,PID3}
VARIABLES activePID1, activePID2, activePID3, readyPID1, readyPID2, readyPID3, waitingPID1, waitingPID2, waitingPID3
INVARIANT activePID1:BOOL & activePID2:BOOL & activePID3:BOOL & (readyPID1:BOOL & readyPID2:BOOL & readyPID3:BOOL) & (waitingPID1:BOOL & waitingPID2:BOOL & waitingPID3:BOOL) & (activePID1:BOOL & activePID2:BOOL & activePID3:BOOL) & (readyPID1:BOOL & readyPID2:BOOL & readyPID3:BOOL) & (waitingPID1:BOOL & waitingPID2:BOOL & waitingPID3:BOOL) & (not(readyPID1=TRUE & waitingPID1=TRUE) & not(readyPID2=TRUE & waitingPID2=TRUE) & not(readyPID3=TRUE & waitingPID3=TRUE)) & (not(activePID1=TRUE & (readyPID1=TRUE or waitingPID1=TRUE)) & not(activePID2=TRUE & (readyPID2=TRUE or waitingPID2=TRUE)) & not(activePID3=TRUE & (readyPID3=TRUE or waitingPID3=TRUE))) & IF activePID1=TRUE THEN 1 ELSE 0 END+IF activePID2=TRUE THEN 1 ELSE 0 END+IF activePID3=TRUE THEN 1 ELSE 0 END<=1 & (not(activePID1=TRUE) & not(activePID2=TRUE) & not(activePID3=TRUE) => not(readyPID1=TRUE) & not(readyPID2=TRUE) & not(readyPID3=TRUE))
INITIALISATION activePID1,activePID2,activePID3 := FALSE,FALSE,FALSE || readyPID1,readyPID2,readyPID3 := FALSE,FALSE,FALSE || waitingPID1,waitingPID2,waitingPID3 := FALSE,FALSE,FALSE
OPERATIONS
rr <-- nr_ready = rr := IF readyPID1=TRUE THEN 1 ELSE 0 END+IF readyPID2=TRUE THEN 1 ELSE 0 END+IF readyPID3=TRUE THEN 1 ELSE 0 END;
newPID1 = SELECT not(activePID1=TRUE) & not(readyPID1=TRUE or waitingPID1=TRUE) THEN waitingPID1 := TRUE END ;
newPID2 = SELECT not(activePID2=TRUE) & not(readyPID2=TRUE or waitingPID2=TRUE) THEN waitingPID2 := TRUE END ;
newPID3 = SELECT not(activePID3=TRUE) & not(readyPID3=TRUE or waitingPID3=TRUE) THEN waitingPID3 := TRUE END ;
delPID1 = SELECT waitingPID1=TRUE THEN waitingPID1 := FALSE END ;
delPID2 = SELECT waitingPID2=TRUE THEN waitingPID2 := FALSE END ;
delPID3 = SELECT waitingPID3=TRUE THEN waitingPID3 := FALSE END ;
readyPID1 = SELECT waitingPID1=TRUE THEN waitingPID1 := FALSE || IF not(activePID1=TRUE) & not(activePID2=TRUE) & not(activePID3=TRUE) THEN activePID1 := TRUE ELSE readyPID1 := TRUE END  END ;
readyPID2 = SELECT waitingPID2=TRUE THEN waitingPID2 := FALSE || IF not(activePID1=TRUE) & not(activePID2=TRUE) & not(activePID3=TRUE) THEN activePID2 := TRUE ELSE readyPID2 := TRUE END  END ;
readyPID3 = SELECT waitingPID3=TRUE THEN waitingPID3 := FALSE || IF not(activePID1=TRUE) & not(activePID2=TRUE) & not(activePID3=TRUE) THEN activePID3 := TRUE ELSE readyPID3 := TRUE END  END ;
swapPID1 = SELECT not(not(activePID1=TRUE) & not(activePID2=TRUE) & not(activePID3=TRUE)) THEN waitingPID1 := bool(waitingPID1=TRUE or activePID1=TRUE) || IF not(readyPID1=TRUE) & not(readyPID2=TRUE) & not(readyPID3=TRUE) THEN activePID1 := FALSE ELSE activePID1 := TRUE || readyPID1 := FALSE END  END ;
swapPID2 = SELECT not(not(activePID1=TRUE) & not(activePID2=TRUE) & not(activePID3=TRUE)) THEN waitingPID2 := bool(waitingPID2=TRUE or activePID2=TRUE) || IF not(readyPID1=TRUE) & not(readyPID2=TRUE) & not(readyPID3=TRUE) THEN activePID2 := FALSE ELSE activePID2 := TRUE || readyPID2 := FALSE END  END ;
swapPID3 = SELECT not(not(activePID1=TRUE) & not(activePID2=TRUE) & not(activePID3=TRUE)) THEN waitingPID3 := bool(waitingPID3=TRUE or activePID3=TRUE) || IF not(readyPID1=TRUE) & not(readyPID2=TRUE) & not(readyPID3=TRUE) THEN activePID3 := FALSE ELSE activePID3 := TRUE || readyPID3 := FALSE END  END 
END