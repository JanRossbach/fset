MACHINE demo
SETS S
VARIABLES a,r
INVARIANT a:POW(S) & r:S<->S &
          a/={} &
          a <: dom(r) &
          !(x).(x:S => x : a\/dom(r))
INITIALISATION a := {} || r := {}
OPERATIONS
conj(x) = SELECT x : S  THEN a := a \/ {x} END;
disj(x) =  SELECT x : a & a/={x} THEN a:= a \ {x} END;
override(x) = SELECT x:S
              THEN ANY y
              WHERE y:S
              THEN
                r := r <+ {x|->y}
              END
              END
END
