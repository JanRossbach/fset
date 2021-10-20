MACHINE scheduler
SETS  PID = {PID1, PID2, PID3}
VARIABLES ready,waiting
INVARIANT ready : POW(PID) & waiting : POW(PID)
INITIALISATION ready := {} || waiting := {}
END