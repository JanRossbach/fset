MACHINE test
SETS
 ID={aa,bb}
CONSTANTS iv,ss
PROPERTIES
 iv:{x|x:ID & x=bb} &
  ss:{aa,bb}
VARIABLES xx
INVARIANT
 xx:ID &
 {aa}<:ID
INITIALISATION xx:=iv
OPERATIONS
  Set(yy) = PRE yy:ID THEN xx:= yy END
END

