MACHINE Banking
SETS ACCOUNTS={A1,A2}; PERSONS={P1,P2}
VARIABLES clientsP1, clientsP2, account_accessP1A1, account_accessP1A2, account_accessP2A1, account_accessP2A2
INVARIANT clientsP1:BOOL & clientsP2:BOOL & (account_accessP1A1:BOOL & account_accessP1A2:BOOL & account_accessP2A1:BOOL & account_accessP2A2:BOOL) & (IF account_accessP1A1=TRUE THEN 1 ELSE 0 END+IF account_accessP2A1=TRUE THEN 1 ELSE 0 END<=1 & IF account_accessP1A2=TRUE THEN 1 ELSE 0 END+IF account_accessP2A2=TRUE THEN 1 ELSE 0 END<=1)
INITIALISATION clientsP1 := FALSE || clientsP2 := FALSE || account_accessP1A1 := FALSE || account_accessP1A2 := FALSE || account_accessP2A1 := FALSE || account_accessP2A2 := FALSE
OPERATIONS
add_clientP1 = SELECT not(clientsP1=TRUE) THEN clientsP1 := TRUE END ;
add_clientP2 = SELECT not(clientsP2=TRUE) THEN clientsP2 := TRUE END ;
rem_clientP1 = SELECT clientsP1=TRUE & IF account_accessP1A1=TRUE THEN 1 ELSE 0 END+IF account_accessP1A2=TRUE THEN 1 ELSE 0 END=0 THEN clientsP1 := FALSE END ;
rem_clientP2 = SELECT clientsP2=TRUE & IF account_accessP2A1=TRUE THEN 1 ELSE 0 END+IF account_accessP2A2=TRUE THEN 1 ELSE 0 END=0 THEN clientsP2 := FALSE END ;
connect_accountP1A1 = SELECT clientsP1=TRUE & IF account_accessP1A1=TRUE THEN 1 ELSE 0 END+IF account_accessP2A1=TRUE THEN 1 ELSE 0 END=0 THEN account_accessP1A1 := TRUE END ;
connect_accountP1A2 = SELECT clientsP1=TRUE & IF account_accessP1A2=TRUE THEN 1 ELSE 0 END+IF account_accessP2A2=TRUE THEN 1 ELSE 0 END=0 THEN account_accessP1A2 := TRUE END ;
connect_accountP2A1 = SELECT clientsP2=TRUE & IF account_accessP1A1=TRUE THEN 1 ELSE 0 END+IF account_accessP2A1=TRUE THEN 1 ELSE 0 END=0 THEN account_accessP2A1 := TRUE END ;
connect_accountP2A2 = SELECT clientsP2=TRUE & IF account_accessP1A2=TRUE THEN 1 ELSE 0 END+IF account_accessP2A2=TRUE THEN 1 ELSE 0 END=0 THEN account_accessP2A2 := TRUE END ;
disconnect_accountP1A1 = SELECT clientsP1=TRUE THEN account_accessP1A1 := FALSE END ;
disconnect_accountP1A2 = SELECT clientsP1=TRUE THEN account_accessP1A2 := FALSE END ;
disconnect_accountP2A1 = SELECT clientsP2=TRUE THEN account_accessP2A1 := FALSE END ;
disconnect_accountP2A2 = SELECT clientsP2=TRUE THEN account_accessP2A2 := FALSE END 
END