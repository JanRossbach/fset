(ns fset.backend
  (:require
   [com.rpl.specter :as s]
   [fset.util :as u]
   [clojure.core.match :refer [match]]
   [lisb.core :refer [eval-ir-formula]]
   [lisb.prob.animator :refer [state-space!]]
   [lisb.translation.util :refer [lisb->ir ir->ast]]
   [lisb.translation.lisb2ir :refer [bmember? bcomp-set]])
  (:import
   de.prob.animator.domainobjects.ClassicalB
   de.prob.animator.domainobjects.FormulaExpand))

(defn get-statespace
  [ir]
  (state-space! (ir->ast ir)))

(defn set-elems
  [ss set-id]
  (eval-ir-formula ss
                   (lisb->ir `(bcomp-set [:x] (bmember? :x ~set-id)))))

(defn get-type
  [ss formula]
  (let [formula-ast (ir->ast formula)
        ee (ClassicalB. formula-ast FormulaExpand/TRUNCATE "")]
    (.getType (.typeCheck ss ee))))

(defn get-possible-var-states
  [ss target-vars other-vars predicate]
  (let [res (eval-ir-formula ss {:tag :comp-set
                                 :identifiers target-vars
                                 :predicate {:tag :exists :identifiers other-vars :predicate predicate}})]
    (if (= res :timeout)
      (throw (ex-info "Call to the API timed out." {:target-var target-vars
                                                    :other-vars other-vars
                                                    :predicate predicate}))
      res)))

;; FIXME
(defn determine-set-elems
  [ir set-to-rewrite d-set-size ss]
  (if true ;;(u/is-deferred? ir set-to-rewrite)
    (map (fn [i] (keyword (str (name set-to-rewrite) i))) (range d-set-size))
    (map keyword (set-elems ss set-to-rewrite))))

(defn gen-pow-set-vars
  [ss set-id var-id]
  (vec (map #(keyword (str (name var-id) (name %))) (set-elems ss set-id))))

(defn extract-ts-from-string
  [type-string]
  (if (re-matches #"POW\((.)*\)" type-string)
    (keyword (subs type-string 4 (dec (count type-string))))
    nil))

(defn api-str->keyword
  [var api-result]
  (into #{}
        (for [r api-result]
          (into #{} (map (fn [s] (keyword (str (name var) s)))  r)))))

;; Mocked
(defn create-app-db
  [_]
  (let [ss {}
        vars '(:active :ready :waiting)
        unroll-vars vars
        unroll-ops '(:new :del :ready :swap)
        sets '(:PID)
        set-elems '(:PID1 :PID2 :PID3)
        elem-map {:active '(:PID1 :PID2 :PID3)
                  :ready '(:PID1 :PID2 :PID3)
                  :waiting '(:PID1 :PID2 :PID3)}]
    {:ss ss
     :sets sets
     :set-elems set-elems
     :elboolvars {:PID1 '(:activePID1 :readyPID1 :waitingPID1)
                  :PID2 '(:activePID2 :readyPID2 :waitingPID2)
                  :PID3 '(:activePID3 :readyPID3 :waitingPID3)}
     :vars vars
     :unroll-vars unroll-vars
     :unroll-ops unroll-ops
     :elem-map elem-map}))

(def app-db (atom (create-app-db {})))

(defn set-element?
  [id]
  (let [set-elems (:set-elems @app-db)]
    (some #(= % id) set-elems)))

(defn involves?
  [ir ids]
  (seq (s/select [(s/walker (fn [w] (some #(= % w) ids)))] ir)))

(defn carrier?
  [id]
  (let [sets (:sets @app-db)]
    (some #(= % id) sets)))

(defn type?
  [expr]
  (match expr
    (_ :guard carrier?) true
    {:tag :power-set :set (_ :guard type?)} true
    {:tag :power1-set :set (_ :guard type?)} true
    {:tag :fin-set :set (_ :guard type?)} true
    {:tag :fin1-set :set (_ :guard type?)} true
    _ false))

(defn unrollable-op?
  [op]
  (some #(= % (:name op)) (:unroll-ops @app-db)))

(defn unrollable-var?
  [id]
  (let [db @app-db]
    (some #(= % id) (:unroll-vars db))))

(defn variable?
  [id]
  (let [vars (:vars @app-db)]
    (some #(= % id) vars)))

(defn enumerable?
  [s]
  true)

(defn boolname
  [var-id el-id]
  (if (and (keyword? var-id) (keyword? el-id))
    (keyword (str (name var-id) (name el-id)))
    (throw (ex-info "One of the ID's given to boolname was not a keyword.
Take care, that all patterns are handled in set->bitvector" {:var-id var-id
                                                             :el-id el-id}))))
(defn replace-param
  [body parameters id]
  (s/setval [(s/walker (fn [w] (some #(= % w) parameters)))] id body))

(defn get-elems-by-id
  [id]
  (let [db @app-db
        elem-map (:elem-map db)]
    (get elem-map id)))

(defn unroll-variable
  [var-id]
  (if (unrollable-var? var-id)
    (map (partial boolname var-id) (get-elems-by-id var-id))
    (list var-id)))

(defn get-all-elems-from-elem
  [_]
  (let [db @app-db]
    (:set-elems db)))

(defn pick-bool-var
  [formulas el-id]
  (filter (fn [formula] (involves? formula (get (:elboolvars @app-db) el-id))) formulas))

(defn calc-op-combinations [op]
  '(:PID1 :PID2 :PID3))
