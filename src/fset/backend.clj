(ns fset.backend
  (:require
   [com.rpl.specter :as s]
   [lisb.core :refer [eval-ir-formula]]
   [lisb.prob.animator :refer [state-space!]]
   [lisb.translation.util :refer [lisb->ir ir->ast]]
   [lisb.translation.lisb2ir :refer [bmember? bcomprehension-set]])
  (:import
   de.prob.animator.domainobjects.ClassicalB
   de.prob.animator.domainobjects.FormulaExpand))

;; SETUP

(defn- TAG [t] (s/path #(= (:tag %) t)))
(def CLAUSES (s/if-path (s/must :ir) [:ir :clauses] [:clauses]))
(defn- CLAUSE [^clojure.lang.Keyword name] (s/path [CLAUSES s/ALL (TAG name)]))
(def VARIABLES (s/path [(CLAUSE :variables) :values]))
(def SETS (s/path [(CLAUSE :sets) :values s/ALL]))

(declare get-statespace)

(def db (atom {}))

;; Define mock caching data

(def scheduler-mock
   {:set->elems {:PID '(:PID1 :PID2 :PID3)}
    :elem->bools {:PID1 '(:activePID1 :readyPID1 :waitingPID1)
                  :PID2 '(:activePID2 :readyPID2 :waitingPID2)
                  :PID3 '(:activePID3 :readyPID3 :waitingPID3)}
    :op->bindings {:nr-ready []
                   :new [[[:pp :PID1]] [[:pp :PID2]] [[:pp :PID3]]]
                   :ready [[[:rr :PID1]] [[:rr :PID2]] [[:rr :PID3]]]
                   :del [[[:pp :PID1]] [[:pp :PID2]] [[:pp :PID3]]]
                   :swap [[[:pp :PID1]] [[:pp :PID2]] [[:pp :PID3]]]}
    :variable->unroll? {:ready true :waiting true}
    :variable->elems {:active '(:PID1 :PID2 :PID3)
                      :ready '(:PID1 :PID2 :PID3)
                      :waiting '(:PID1 :PID2 :PID3)}})

;; HELPERS

(defn- get-statespace
  [ir]
  (state-space! (ir->ast ir)))

(defn- interpret-animator-result
  [result]
  (sort (map keyword result)))

(defn- get-set-elems
  [set-id]
  (let [ss (:ss @db)]
    (interpret-animator-result
     (eval-ir-formula ss
                      (lisb->ir `(bcomprehension-set [:x] (bmember? :x ~set-id)))))))

(defn- get-type
  [formula]
  (let [ss (:ss @db)
        formula-ast (ir->ast formula)
        ee (ClassicalB. formula-ast FormulaExpand/TRUNCATE "")]
    (.getType (.typeCheck ss ee))))

(defn- get-possible-var-states
  [ss target-vars other-vars predicate]
  (let [res (eval-ir-formula ss {:tag :comprehension-set
                                 :identifiers target-vars
                                 :predicate {:tag :exists :identifiers other-vars :predicate predicate}})]
    (if (= res :timeout)
      (throw (ex-info "Call to the API timed out." {:target-var target-vars
                                                    :other-vars other-vars
                                                    :predicate predicate}))
      res)))

(defn- determine-set-elems
  [set-to-rewrite d-set-size]
  (if true ;;(u/is-deferred? ir set-to-rewrite)
    (map (fn [i] (keyword (str (name set-to-rewrite) i))) (range d-set-size))
    (map keyword (get-set-elems set-to-rewrite))))

(defn- gen-pow-set-vars
  [ss set-id var-id]
  (vec (map #(keyword (str (name var-id) (name %))) (get-set-elems set-id))))

(defn- extract-ts-from-string
  [type-string]
  (if (re-matches #"POW\((.)*\)" type-string)
    (keyword (subs type-string 4 (dec (count type-string))))
    nil))

(defn- api-str->keyword
  [var api-result]
  (into #{}
        (for [r api-result]
          (into #{} (map (fn [s] (keyword (str (name var) s)))  r)))))

(defn- finite-var?
  [_]
  true)

(defn- involves?
  [ir ids]
  (seq (s/select [(s/walker (fn [w] (some #(= % w) ids)))] ir)))

;; Public API

(defn setup-backend ;; Call this before testing the backend isolated without an actual translation
  [ir]
  (reset! db (assoc
                 scheduler-mock ;; Hard code the mock. Set to {} when ready to use normally
                 :ir ir
                 :ss (get-statespace ir))))

(defn carrier?
  [id]
  (seq (s/select [SETS :identifier #(= % id)] (:ir @db))))

(defn create-boolname [& ids]
  (keyword (apply str (map name (flatten ids)))))

(defn set-element?
  [id]
  (seq (s/select [:set->elems s/MAP-VALS s/ALL #(= % id)] @db)))

(defn variable?
  [id]
  (seq (s/select [VARIABLES s/ALL #(= % id)] (:ir @db))))

(defn unrollable-var?
  [var-id]
  (let [c @db
        m (:variable->unroll? c)]
    (if (contains? m var-id)
      (get m var-id)
      (if (variable? var-id)
        (if (finite-var? var-id)
          (do (swap! db assoc :variable->unroll? (assoc m var-id :hello)) true)
          (do (swap! db assoc :variable->unroll? (assoc m var-id false)) false))
        false))))

(defn enumerable?
  [s]
  true)

(defn- varid->elems
  [id]
  (let [elem-map (:variable->elems @db)]
    (get elem-map id)))

(defn unroll-variable
  [var-id]
  (if (unrollable-var? var-id)
    (map (partial create-boolname var-id) (varid->elems var-id))
    (list var-id)))

(defn get-all-elems-from-elem
  [elem-id]
  (->> (:set->elems @db)
       (into [])
       (map second)
       (filter (fn [elems] (some #(= % elem-id) elems)))
       (first)))

(defn pick-bool-var
  [formulas el-id]
  (filter (fn [formula] (involves? formula (get (:elem->bools @db) el-id))) formulas))

(defn get-op-combinations [op-id]
  (let [db @db
        op-combs (:op->bindings db)]
    (get op-combs op-id)))
