MACHINE scheduler_rewritten
VARIABLES activePID1, activePID2, activePID3, readyPID1, readyPID2, readyPID3, waitingPID1, waitingPID2, waitingPID3
INVARIANT
  activePID1:BOOL & activePID2:BOOL & activePID3:BOOL &
  readyPID1:BOOL & readyPID2:BOOL & readyPID3:BOOL &
  waitingPID1:BOOL & waitingPID2:BOOL & waitingPID3:BOOL &
  (bool(readyPID1=TRUE&waitingPID1=TRUE)=FALSE &
  bool(readyPID2=TRUE&waitingPID2=TRUE)=FALSE &
  bool(readyPID3=TRUE&waitingPID3=TRUE)=FALSE) &
  bool(activePID1=TRUE & bool(readyPID1=TRUE or waitingPID1=TRUE)=TRUE)=FALSE &
  bool(activePID2=TRUE & bool(readyPID2=TRUE or waitingPID2=TRUE)=TRUE)=FALSE &
  bool(activePID3=TRUE & bool(readyPID3=TRUE or waitingPID3=TRUE)=TRUE)=FALSE &
  (card({x|x : {activePID1, activePID2, activePID3} & x=TRUE}) <=1) &
  ((bool(activePID1=TRUE)=FALSE & bool(activePID2=TRUE)=FALSE & bool(activePID3=TRUE)=FALSE)
  => (bool(readyPID1=TRUE)=FALSE & bool(readyPID2=TRUE)=FALSE & bool(readyPID3=TRUE)=FALSE))
INITIALISATION
   activePID1:=FALSE || activePID2 := FALSE || activePID3 := FALSE ||
   readyPID1 := FALSE || readyPID2 := FALSE || readyPID3 := FALSE ||
   waitingPID1 := FALSE || waitingPID2 := FALSE || waitingPID3 := FALSE
OPERATIONS
newPID1 =
    SELECT
        activePID1=FALSE &
        readyPID1=FALSE &
        waitingPID1=FALSE
    THEN
        waitingPID1:=TRUE
    END;
newPID2 =
    SELECT
        activePID2=FALSE &
        readyPID2=FALSE &
        waitingPID2=FALSE
    THEN
        waitingPID2:=TRUE
    END;
newPID3 =
    SELECT
        activePID3=FALSE &
        readyPID3=FALSE &
        waitingPID3=FALSE
    THEN
        waitingPID3:=TRUE
    END
END