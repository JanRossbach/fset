(ns fset.core-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [fset.core :refer [transform]]))


(def empty-cfg {:deferred-size 3 :max-transform-size 10})

(def empty-machine-lisb '(machine (machine-variant) (machine-header :Empty [])))

(def empty-machine-str "MACHINE Empty\nEND")

(deftest empty-machine-test
  (testing "The empty machine should not be changed in any way"
    (is (= empty-machine-str (transform empty-cfg empty-machine-lisb)))))


(def scheduler-cfg {:deferred-size 3 :max-transform-size 10})

(def scheduler-machine-lisb
  '(machine (machine-variant) (machine-header :scheduler []) (sets (deferred-set :PID)) (variables :active :ready :waiting) (invariant (and (contains? (pow :PID) :active) (contains? (pow :PID) :ready) (contains? (pow :PID) :waiting) (subset? :active :PID) (subset? :ready :PID) (subset? :waiting :PID) (= (intersection :ready :waiting) #{}) (= (intersection :active (union :ready :waiting)) #{}) (<= (count :active) 1) (=> (= :active #{}) (= :ready #{})))) (init (parallel-substitution (assign :active #{}) (assign :ready #{}) (assign :waiting #{}))) (operations (operation [:rr] :nr_ready [] (assign :rr (count :ready))) (operation [] :new [:pp] (select (and (contains? :PID :pp) (not (contains? :active :pp)) (not (contains? (union :ready :waiting) :pp))) (assign :waiting (union :waiting #{:pp})))) (operation [] :del [:pp] (select (contains? :waiting :pp) (assign :waiting (- :waiting #{:pp})))) (operation [] :ready [:rr] (select (contains? :waiting :rr) (parallel-substitution (assign :waiting (- :waiting #{:rr})) (if-sub (= :active #{}) (assign :active #{:rr}) (assign :ready (union :ready #{:rr})))))) (operation [] :swap [] (select (not= :active #{}) (parallel-substitution (assign :waiting (union :waiting :active)) (if-sub (= :ready #{}) (assign :active #{}) (any [:pp] (contains? :ready :pp) (parallel-substitution (assign :active #{:pp}) (assign :ready (- :ready #{:pp})))))))))))

(def scheduler-machine-str
  "MACHINE scheduler\nSETS PID\nVARIABLES active, ready, waiting\nINVARIANT active:POW(PID) & ready:POW(PID) & waiting:POW(PID) & active<:PID & ready<:PID & waiting<:PID & ready/\\waiting={} & active/\\(ready\\/waiting)={} & card(active)<=1 & (active={} => ready={})\nINITIALISATION active := {} || ready := {} || waiting := {}\nOPERATIONS\nrr <-- nr_ready = rr := card(ready);\nnew(pp) = SELECT pp:PID & pp/:active & pp/:ready\\/waiting THEN waiting := waiting\\/{pp} END ;\ndel(pp) = SELECT pp:waiting THEN waiting := waiting-{pp} END ;\nready(rr) = SELECT rr:waiting THEN waiting := waiting-{rr} || IF active={} THEN active := {rr} ELSE ready := ready\\/{rr} END  END ;\nswap = SELECT active/={} THEN waiting := waiting\\/active || IF ready={} THEN active := {} ELSE ANY pp WHERE pp:ready THEN active := {pp} || ready := ready-{pp} END  END  END \nEND")

(deftest scheduler-machine-test
  (testing "The scheduler machine should be changed in some way"
    (is (not= scheduler-machine-str (transform scheduler-cfg scheduler-machine-lisb)))))
