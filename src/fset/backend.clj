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
     :op-comps {:nr-ready []
                :new [[[:pp :PID1]] [[:pp :PID2]] [[:pp :PID3]]]
                :ready [[[:rr :PID1]] [[:rr :PID2]] [[:rr :PID3]]]
                :del [[[:pp :PID1]] [[:pp :PID2]] [[:pp :PID3]]]
                :swap [[[:pp :PID1]] [[:pp :PID2]] [[:pp :PID3]]]}
     :unroll-vars unroll-vars
     :unroll-ops unroll-ops
     :elem-map elem-map}))

(def app-cache (atom (create-app-db {})))

(defn set-element?
  [id]
  (let [set-elems (:set-elems @app-cache)]
    (some #(= % id) set-elems)))

(defn involves?
  [ir ids]
  (seq (s/select [(s/walker (fn [w] (some #(= % w) ids)))] ir)))

(defn carrier?
  [id]
  (let [sets (:sets @app-cache)]
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
  (some #(= % (:name op)) (:unroll-ops @app-cache)))

(defn unrollable-var?
  [id]
  (let [db @app-cache]
    (some #(= % id) (:unroll-vars db))))

(defn variable?
  [id]
  (let [vars (:vars @app-cache)]
    (some #(= % id) vars)))

(defn enumerable?
  [s]
  true)

(defn create-boolname
  [& ids]
  (keyword (apply str (map name ids))))

(defn get-elems-by-id
  [id]
  (let [db @app-cache
        elem-map (:elem-map db)]
    (get elem-map id)))

(defn unroll-variable
  [var-id]
  (if (unrollable-var? var-id)
    (map (partial create-boolname var-id) (get-elems-by-id var-id))
    (list var-id)))

(defn get-all-elems-from-elem
  [_]
  (let [db @app-cache]
    (:set-elems db)))

(defn pick-bool-var
  [formulas el-id]
  (filter (fn [formula] (involves? formula (get (:elboolvars @app-cache) el-id))) formulas))

(defn calc-op-combinations [op-id]
  (let [db @app-cache
        op-combs (:op-comps db)]
    (get op-combs op-id)))
