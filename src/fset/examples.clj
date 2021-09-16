(ns fset.examples
  (:require
   [lisb.translation.util :refer [lisb->b]]
   [lisb.translation.lisb2ir :refer [lisb->ir]]
   [lisb.high-level :refer [save-mch!]]
   [lisb.fset.core :refer [print-transform! target-dir]]))

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

  (print-transform! (lisb->ir scheduler))

  (save-mch! (lisb->ir scheduler) (str target-dir "scheduler-lisb.mch"))

  (lisb->b scheduler)

)
