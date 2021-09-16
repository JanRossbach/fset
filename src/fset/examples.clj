(ns fset.examples
  (:require
   [lisb.translation.util :refer [lisb->b lisb->ir]]
   [fset.util :refer [save-mch!]]))

(def target-dir "resources/machines/lisb/target/")

(def scheduler
  '(machine
    (machine-variant)
    (machine-header :scheduler [])
    (sets (deferred-set :PID))
    (variables :active :ready :waiting)
    (invariant (and
                (member? :active (pow :PID))
                (member? :ready (pow :PID))
                (member? :waiting (pow :PID))
                (subset? :active :PID)
                (subset? :ready :PID)
                (subset? :waiting :PID)))
    (init (assign :active #{} :ready #{} :waiting #{}))
    (operations
     (operation [:rr] :nr_ready [] (assign :rr (count :ready)))
     (operation [] :new [:pp] (select (and
                                      (member? :pp :PID)
                                      (not (member? :pp :active))
                                      (not (member? :pp (union :ready :waiting))))
                                      (assign :waiting (union :waiting #{:pp}))))
     (operation [] :del [:pp] (select
                               (member? :pp :waiting)
                               (assign :waiting (difference :waiting #{:pp})))))))

(comment

  (save-mch! (lisb->ir scheduler) (str target-dir "scheduler-lisb.mch"))

  (lisb->b scheduler)

)
