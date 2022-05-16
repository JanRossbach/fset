(ns fset.backend.lisb-util
  (:require
   [lisb.prob.animator :refer [state-space! get-result]]
   [lisb.translation.util :refer [ir->ast]])
  (:import
   de.prob.animator.domainobjects.ClassicalB
   de.prob.animator.domainobjects.FormulaExpand))

(defn get-type
   [ss formula]
    (let [formula-ast (ir->ast formula)
          ee (ClassicalB. formula-ast FormulaExpand/TRUNCATE "")]
      (.getType (.typeCheck ss ee))))

(def get-statespace
  (memoize (fn [ir] (state-space! (ir->ast ir)))))

(defn model-check [ir]
  (let [ss (get-statespace ir)
        cmd (de.prob.animator.command.ModelCheckingStepCommand.
             999999 ; max number of states
             120000 ; time (in msecs?)
             de.prob.check.ModelCheckingOptions/DEFAULT)]
    (.execute ss cmd)
    (let [statsobj (.getStats cmd)]
      {:result (.getResult cmd)
       :states (.getNrTotalNodes statsobj)
       :transitions (.getNrTotalTransitions statsobj)})))

(defn predicate?
  [ss expr]
  (= "predicate" (get-type ss expr)))

(defn intexpr?
  [ss expr]
  (= "INTEGER" (get-type ss expr)))

(defn eval-constant-formula [constant-state ir-formula]
  (let [formula (de.prob.animator.domainobjects.ClassicalB. (ir->ast ir-formula) de.prob.animator.domainobjects.FormulaExpand/EXPAND)
        res-map (.evalFormulas constant-state [formula])]
    (get-result (get res-map formula))))
