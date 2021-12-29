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
    END;
    rem_client =
	ANY p
	WHERE p : PERSONS & p : clients & card(account_access[{p}])=0
	THEN clients := clients - {p}
    END;
    connect_account =
	ANY c,a
	WHERE c : clients & a : ACCOUNTS & card(account_access~[{a}]) = 0
	THEN account_access := account_access \/ {c|->a}
    END;
    disconnect_account =
	ANY c,a
	WHERE c : clients & a : ACCOUNTS
	THEN account_access := account_access - {c|->a}
    END
END