MACHINE scheduler
SETS PID={P1,P2}
VARIABLES activeP1, activeP2, readyP1, readyP2, waitingP1, waitingP2
INVARIANT activeP1:BOOL & activeP2:BOOL & readyP1:BOOL & readyP2:BOOL & waitingP1:BOOL & waitingP2:BOOL & TRUE=TRUE & TRUE=TRUE & TRUE=TRUE & TRUE=TRUE & TRUE=TRUE & TRUE=TRUE & (not(readyP1=TRUE & waitingP1=TRUE) & not(readyP2=TRUE & waitingP2=TRUE)) & (not(activeP1=TRUE & (readyP1=TRUE or waitingP1=TRUE)) & not(activeP2=TRUE & (readyP2=TRUE or waitingP2=TRUE))) & IF activeP1=TRUE THEN 1 ELSE 0 END+IF activeP2=TRUE THEN 1 ELSE 0 END<=1 & (not(activeP1=TRUE) & not(activeP2=TRUE) => not(readyP1=TRUE) & not(readyP2=TRUE))
INITIALISATION activeP1 := FALSE || activeP2 := FALSE || readyP1 := FALSE || readyP2 := FALSE || waitingP1 := FALSE || waitingP2 := FALSE
OPERATIONS
rr <-- nr_ready = rr := card(IF readyP1=TRUE THEN {P1} ELSE {} END\/IF readyP2=TRUE THEN {P2} ELSE {} END);
newP1 = SELECT not(activeP1=TRUE) & not(readyP1=TRUE or waitingP1=TRUE) THEN waitingP1 := TRUE END ;
newP2 = SELECT not(activeP2=TRUE) & not(readyP2=TRUE or waitingP2=TRUE) THEN waitingP2 := TRUE END ;
delP1 = SELECT waitingP1=TRUE THEN waitingP1 := FALSE END ;
delP2 = SELECT waitingP2=TRUE THEN waitingP2 := FALSE END ;
readyP1 = SELECT waitingP1=TRUE THEN waitingP1 := FALSE || IF not(activeP1=TRUE) & not(activeP2=TRUE) THEN activeP1 := TRUE || activeP2 := FALSE ELSE readyP1 := TRUE END  END ;
readyP2 = SELECT waitingP2=TRUE THEN waitingP2 := FALSE || IF not(activeP1=TRUE) & not(activeP2=TRUE) THEN activeP1 := FALSE || activeP2 := TRUE ELSE readyP2 := TRUE END  END ;
swapP1 = SELECT readyP1=TRUE & not(not(activeP1=TRUE) & not(activeP2=TRUE)) THEN waitingP1 := bool(waitingP1=TRUE or activeP1=TRUE) || waitingP2 := bool(waitingP2=TRUE or activeP2=TRUE) || IF not(readyP1=TRUE) & not(readyP2=TRUE) THEN activeP1 := FALSE || activeP2 := FALSE ELSE SELECT readyP1=TRUE THEN activeP1 := TRUE || activeP2 := FALSE || readyP1 := FALSE END  END  END ;
swapP2 = SELECT readyP2=TRUE & not(not(activeP1=TRUE) & not(activeP2=TRUE)) THEN waitingP1 := bool(waitingP1=TRUE or activeP1=TRUE) || waitingP2 := bool(waitingP2=TRUE or activeP2=TRUE) || IF not(readyP1=TRUE) & not(readyP2=TRUE) THEN activeP1 := FALSE || activeP2 := FALSE ELSE SELECT readyP2=TRUE THEN activeP1 := FALSE || activeP2 := TRUE || readyP2 := FALSE END  END  END 
END