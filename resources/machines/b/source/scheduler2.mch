MACHINE scheduler
SETS
  PID = {PID1, PID2, PID3}
VARIABLES activePID1, activePID2, readyPID3, readyPID1, readyPID2, readyPID3, waitingPID1, waitingPID2, waitingPID3
PROPERTIES activePID1 : BOOL & active_PID2 : BOOL & active_PID3 : BOOL
INVARIANT active_PID1 <=> bool(PID1 : active) &
          active_PID2 <=> bool(PID2 : active) &
          active_PID3 <=> bool(PID3 : active)
INITIALISATION active_PID1:=FALSE || active_PID2:=FALSE || active_PID2:=FALSE
