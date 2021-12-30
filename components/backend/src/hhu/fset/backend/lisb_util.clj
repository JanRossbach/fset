(ns hhu.fset.backend.lisb-util
  (:require
   [lisb.core :refer [eval-ir-formula]]
   [lisb.prob.animator :refer [state-space!]]
   [lisb.translation.util :refer [ir->ast b->ir]]
   [lisb.translation.lisb2ir :refer [bmember? bcomprehension-set bexists band b=]])
  (:import
   de.prob.animator.domainobjects.ClassicalB
   de.prob.animator.domainobjects.FormulaExpand))

(defn get-type
   [ss formula]
    (let [formula-ast (ir->ast formula)
          ee (ClassicalB. formula-ast FormulaExpand/TRUNCATE "")]
      (.getType (.typeCheck ss ee))))

(defn get-statespace
  [ir] (state-space! (ir->ast ir)))

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
