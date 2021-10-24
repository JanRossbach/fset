MACHINE scheduler
SETS
  PID = {PID1, PID2, PID3}
VARIABLES active_PID1, active_PID2, active_PID3
PROPERTIES actvie_PID1 : BOOL & active_PID2 : BOOL & active_PID3 : BOOL
INVARIANT active_PID1 <=> bool(PID1 : active) &
          active_PID2 <=> bool(PID2 : active) &
          active_PID3 <=> bool(PID3 : active)
INITIALISATION active_PID1:=FALSE || active_PID2:=FALSE || active_PID2:=FALSE
