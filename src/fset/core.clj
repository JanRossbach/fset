(ns fset.core
  (:require
   [fset.backend.predicates :refer [transform-predicate]]
   [clojure.spec.alpha :refer [valid? explain]]
   [lisb.core :refer [eval-ir-formula]]
   [lisb.prob.animator :refer [state-space!]]
   [lisb.translation.util :refer [lisb->ir]]
   [lisb.translation.lisb2ir :refer [bcomp-set bmember?]]
   [fset.spec :refer :all]
   [fset.util :as util]
   [fset.backend.variables :refer [generate-variables]]))

(defrecord universe [ir ^clojure.lang.Keyword target-set ^long set-size variables])

(defn transform-variables
  [^universe u]
  (let [new-vars (util/get-new-var-ids u)
        old-vars (util/get-old-var-ids u)]
    (-> u
        (util/rm-vars old-vars)
        (util/add-vars new-vars))))

(defn transform-properties
  [^universe u]
  (let [properties (util/get-properties u)]
    (if properties
      (util/set-properties u {:tag :properties
                              :predicate (transform-predicate u (:predicate properties))})
      u)))

(defn transform-invariant
  [^universe u]
  (let [invariant (util/get-invariant u)]
    (if invariant
      (util/set-invariant u {:tag :invariants
                              :predicate (transform-predicate u (:predicate invariant))})
      u)))

(defn count-enum-set
  [ss set-id]
  (count (eval-ir-formula ss (lisb->ir `(bcomp-set [:x] (bmember? :x ~set-id))))))

(defn calc-set-size
  [ir ts m ds]
  (if (< m ds)
    (throw (ex-info "The deferred set size is larger than the max size!" {:deferred-size ds :max-size m}))
    (if (util/is-enumerated? ir ts)
      (let [c (count-enum-set (state-space! (lisb.translation.util/ir->ast ir)) ts)]
        (if (< c m)
          c
          (throw (ex-info "The given set is bigger than the given maximum size!" {:set ts
                                                                                  :ir ir
                                                                                  :size c}))))
      ds)))

(defn validate
  "Debugging helper function using clojure spec."
  [u debug]
  (if debug
    (if (valid? :fset/universe u)
      u
      (do (explain :fset/universe u)
          (throw (ex-info "Something went wrong creating the universe. Spec did not match!"
                          {:universe u}))))
    u))

(defn transform
  "The entry Function that does the transformation."
  [max-size deferred-set-size debug ir set-to-rewrite] ;; Parameter order is chosen to make it easy to partial away the config stuff.
  (let [u (->universe ir set-to-rewrite (calc-set-size ir set-to-rewrite max-size deferred-set-size) {})]
    (-> u
     (generate-variables)
     (validate debug)
     (transform-variables)
     (transform-invariant)
     (transform-properties)
     (util/clear-empty-sets))))
