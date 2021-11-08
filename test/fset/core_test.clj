(ns fset.core-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [lisb.translation.util :refer [ir->b b->ir]]
   [fset.core :refer [boolencode set->bitvector init-db]]))


(def empty-ir {:tag :machine, :clauses '(), :name :Empty})

(deftest empty-machine-test
  (testing "The empty machine does not contain the :nil set and should throw an exception."
    (is (= empty-ir (boolencode empty-ir)))))

(def scheduler-ir (b->ir (slurp "resources/test/scheduler.mch")))

(deftest scheduler-machine-test
  (testing "The scheduler machine should be changed in some way"
    (is (not= scheduler-ir (boolencode scheduler-ir)))))

(deftest valid-B-machine-test
  (testing "After the transformation the IR can be translated into a B machine."
    (is (string? (ir->b (boolencode scheduler-ir))))))

(def elems '(:PID1 :PID2 :PID3))

(def empty-set '({:tag :equal, :left :TRUE, :right :FALSE} {:tag :equal, :left :TRUE, :right :FALSE} {:tag :equal, :left :TRUE, :right :FALSE}))
(def active '({:tag :equal, :left :activePID1, :right :TRUE} {:tag :equal, :left :activePID2, :right :TRUE} {:tag :equal, :left :activePID3, :right :TRUE}))
(def singleton '({:tag :equal, :left :TRUE, :right :FALSE} {:tag :equal, :left :TRUE, :right :TRUE} {:tag :equal, :left :TRUE, :right :FALSE}))
(def union '({:tag :or, :predicates ({:tag :equal, :left :activePID1, :right :TRUE} {:tag :equal, :left :waitingPID1, :right :TRUE})} {:tag :or, :predicates ({:tag :equal, :left :activePID2, :right :TRUE} {:tag :equal, :left :waitingPID2, :right :TRUE})} {:tag :or, :predicates ({:tag :equal, :left :activePID3, :right :TRUE} {:tag :equal, :left :waitingPID3, :right :TRUE})}))
(def intersection '({:tag :and, :predicates ({:tag :equal, :left :activePID1, :right :TRUE} {:tag :equal, :left :waitingPID1, :right :TRUE})} {:tag :and, :predicates ({:tag :equal, :left :activePID2, :right :TRUE} {:tag :equal, :left :waitingPID2, :right :TRUE})} {:tag :and, :predicates ({:tag :equal, :left :activePID3, :right :TRUE} {:tag :equal, :left :waitingPID3, :right :TRUE})}))
(def difference '({:tag :and, :predicates ({:tag :equal, :left :activePID1, :right :TRUE} {:tag :not, :predicate {:tag :equal, :left :waitingPID1, :right :TRUE}})} {:tag :and, :predicates ({:tag :equal, :left :activePID2, :right :TRUE} {:tag :not, :predicate {:tag :equal, :left :waitingPID2, :right :TRUE}})} {:tag :and, :predicates ({:tag :equal, :left :activePID3, :right :TRUE} {:tag :not, :predicate {:tag :equal, :left :waitingPID3, :right :TRUE}})}))

(deftest bitvector-translation-test
  (is (= active (set->bitvector elems :active)))
  (is (= empty-set (set->bitvector elems #{})))
  (is (= singleton (set->bitvector elems #{:PID2})))
  (is (= union (set->bitvector elems {:tag :union :sets '(:active :waiting)})))
  (is (= intersection (set->bitvector elems {:tag :intersection :sets '(:active :waiting)})))
  (is (= difference (set->bitvector elems {:tag :difference :sets '(:active :waiting)}))))
