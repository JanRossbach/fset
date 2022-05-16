MACHINE b2sat_test
SETS S={s1,s2}
VARIABLES ss1, ss2, x
INVARIANT ss1:BOOL & ss2:BOOL & TRUE=TRUE & x:S
INITIALISATION x::S || ss1 := FALSE || ss2 := FALSE
OPERATIONS
add_x = SELECT not((x=s1 => ss1=TRUE) & (x=s2 => ss2=TRUE)) THEN ss1 := bool(ss1=TRUE or x=s1) || ss2 := bool(ss2=TRUE or x=s2) END 
END