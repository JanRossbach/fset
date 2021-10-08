(ns fset.core
  (:require
   [fset.predicates :refer [transform-predicate]]
   [clojure.spec.alpha :refer [valid? explain]]
   [fset.spec :refer :all]
   [fset.util :as util]
   [fset.variables :refer [generate-variables]]))

(defrecord universe [ir ^clojure.lang.Keyword set-to-rewrite ^long max-size ^long deferred-set-size variables])

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

(defn validate
  "Debugging helper function using clojure spec."
  [u]
  (if (valid? :fset/universe u)
    u
    (do (explain :fset/universe u)
        (throw (ex-info "Something went wrong creating the universe. Spec did not match!"
                        {:universe u})))))

(defn transform
  "The entry Function that does the transformation."
  [max-size deferred-set-size ir set-to-rewrite] ;; Parameter order is chosen to make it easy to partial away the config stuff.
  (-> (->universe ir set-to-rewrite max-size deferred-set-size (generate-variables ir))
      (validate)
      (transform-variables)
      (transform-invariant)
      (transform-properties)
      (:ir)))
