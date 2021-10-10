(ns fset.core
  (:require
   [fset.backend :as b]
   [clojure.spec.alpha :as spec]
   [fset.util :as util]))

;; lisb specs


(spec/def :lisb/tag keyword?)
(spec/def :lisb/name keyword?)
(spec/def :lisb/parameters vector?)

(spec/def :lisb/variant (spec/keys :req-un [:lisb/tag]))
(spec/def :lisb/header (spec/keys :req-un [:lisb/tag :lisb/name :lisb/parameters]))
(spec/def :lisb/predicate map?)
(spec/def :lisb/invariant (spec/keys :req-un [:lisb/tag :lisb/predicate]))
(spec/def :lisb/properties map?)
(spec/def :lisb.operation/operation map?)
(spec/def :lisb.operation/operations (spec/coll-of :lisb.operation/operation))
(spec/def :lisb/operations (spec/keys :req-un [:lisb/tag :lisb.operation/operations]))
(spec/def :lisb/constants map?)
(spec/def :lisb/sets map?)
(spec/def :lisb/constraints map?)
(spec/def :lisb/clause (spec/or :invariant :lisb/invariant
                                :sets :lisb/sets
                                :properties :lisb/properties
                                :constants :lisb/constants
                                :operations :lisb/operations))
(spec/def :lisb/clauses (spec/or :nil nil?
                                 :clause (spec/coll-of :lisb/clause)))

(spec/def :lisb/ir (spec/keys :req-un [:lisb/tag :lisb/variant :lisb/header :lisb/clauses]))

;; fset specs

(spec/def :fset/id keyword?)
(spec/def :fset/elems (spec/coll-of :fset/id))
(spec/def :fset/target-set (spec/keys :req-un [:fset/id :fset/elems]))
(spec/def :fset/variables (spec/map-of keyword? (spec/coll-of keyword?)))

(spec/def :fset/universe (spec/keys :req-un [:lisb/ir
                                             :fset/target-set
                                             :fset/variables]))


(defrecord universe [ir ^clojure.lang.Keyword target-set variables])

(defmulti resolve-type (fn [_ _ t] (:tag t)))

(defmethod resolve-type :default
  [_ var-id _]
  [var-id])

(defmethod resolve-type :power-set
  [u var-id _]
  (let [n (count (:elems (:target-set u)))
        {:keys [id]} (:target-set u)]
    (vec (map (fn [i] (keyword (str (name var-id) (name id) i))) (range n)))))

(defn- is-relevant?
  [u [_ type]]
  (let [set-to-rewrite (:id (:target-set u))]
    (= (:set type) set-to-rewrite)))

(defn- generate-variable
  [u [var-id type]]
  (let [old-variables (:variables u)
        new-variables (assoc old-variables var-id (resolve-type u var-id type))]
    (assoc u :variables new-variables)))

(defn generate-variables
  "Statically analyzes the IR and generates a map of bindings from variable id's that
  need to be rewritten to coll of boolean ids corresponding to the variable in the new machine."
  [u]
  (let [vars (util/get-vars u)
        typedefs (map (fn [v] [v,(first (util/get-type u v))]) vars)
        relevant-vars (filter (partial is-relevant? u) typedefs)]
    (reduce generate-variable u relevant-vars)))

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
                              :predicate (b/transform-predicate u (:predicate properties))})
      u)))

(defn transform-invariant
  [^universe u]
  (let [invariant (util/get-invariant u)]
    (if invariant
      (util/set-invariant u {:tag :invariants
                              :predicate (b/transform-predicate u (:predicate invariant))})
      u)))

(defn validate
  "Debugging helper function using clojure spec."
  [u debug]
  (if debug
    (if (spec/valid? :fset/universe u)
      u
      (do (spec/explain :fset/universe u)
          (throw (ex-info "Something went wrong creating the universe. Spec did not match!"
                          {:universe u}))))
    u))

(defn determine-set-elems
  [ir set-to-rewrite max-size d-set-size]
  (if (util/is-deferred? ir set-to-rewrite)
    (map (fn [i] (keyword (str (name set-to-rewrite) i))) (range d-set-size))
    (map keyword (b/set-elems ir set-to-rewrite))))

(defn transform
  "The entry Function that does the transformation."
  [max-size deferred-set-size debug ir set-to-rewrite] ;; Parameter order is chosen to make it easy to partial away the config stuff.
  (let [u (->universe ir {:id set-to-rewrite :elems (determine-set-elems ir set-to-rewrite max-size deferred-set-size)} {})]
    (-> u
     (generate-variables)
     (validate debug)
     (transform-variables)
     (transform-invariant)
     (transform-properties)
     (util/clear-empty-sets))))
