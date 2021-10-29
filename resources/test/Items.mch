MACHINE CountSet
CONSTANTS initial_items
PROPERTIES initial_items = 1..10
VARIABLES count, items
INVARIANT count : NATURAL & items <: initial_items &
           (items={} => count = card(initial_items))
INITIALISATION count:=0 || items := initial_items
OPERATIONS

count_item(i) =
           SELECT i:items
           THEN
              count:=count+1 ||
              items := items - {i}
           END
END
