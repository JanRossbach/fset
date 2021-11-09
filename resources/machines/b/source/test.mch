MACHINE scheduler

SETS
    PID = {PID1, PID2, PID3}

VARIABLES active, ready, waiting

INVARIANT
           active : POW(PID) & ready : POW(PID) & waiting: POW(PID) &/* the types */
           (ready /\ waiting) = {} &
           active /\ (ready \/ waiting) = {} &
           ((active = {})  => (ready = {})) &
           (waiting - {PID1}) = {PID2}

INITIALISATION
    active := {} || ready := {} || waiting := {}

OPERATIONS

rr <-- nr_true = rr:=%x.(x:{bool(active={})}|IF x=TRUE THEN 1 ELSE 0 END);
rr <-- nr_ready = rr:= IF (PID1:active) THEN 1 ELSE 0 END + IF PID2:active THEN 1 ELSE 0 END + IF PID3:active THEN 1 ELSE 0 END;
new(pp) =
    SELECT
        pp : PID  &
        pp /: active &
        pp /: (ready \/ waiting)
    THEN
        waiting := (waiting \/ { pp })
    END;

del(pp) =
    SELECT
        pp : waiting
    THEN
        waiting := waiting - { pp }
    END;

ready(rr) =
        SELECT
                rr : waiting
        THEN
                waiting := (waiting - {rr}) ||
                IF (active = {}) THEN
                   active := {rr}
                ELSE
                   ready := ready \/ {rr}
                END
        END;

swap =
        SELECT
                active /= {}
        THEN
                waiting := (waiting \/ active) ||
                IF (ready = {}) THEN
                   active := {}
                ELSE
                   ANY pp WHERE pp : ready
                   THEN
                       active := {pp} ||
                       ready := ready - {pp}
                   END
                END
        END
END