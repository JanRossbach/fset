MACHINE Banking
SETS ACCOUNTS; PERSONS
VARIABLES clients, account_access
INVARIANT
   clients <: PERSONS &
   account_access : clients <-> ACCOUNTS &
   ! (x) . (x:ACCOUNTS => card(account_access~[{x}])<=1)
INITIALISATION clients, account_access := {},{}
OPERATIONS
    add_client = 
	ANY p
	WHERE p : PERSONS & p/:clients
	THEN clients := clients \/ {p}
    END
END