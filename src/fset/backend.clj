(ns fset.backend
  (:require
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

(defn make-var
  [ss var-id]
  {:id var-id
   })
