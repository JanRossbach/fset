(ns fset.core
  (:require
   [fset.backend :as b]
   [fset.transformations :as transform]
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
(spec/def :lisb/statespace #(= (type %) de.prob.statespace.StateSpace))

(spec/def :fset/universe (spec/keys :req-un [:lisb/ir
                                             :lisb/statespace
                                             :fset/target-set
                                             :fset/variables]))


(defrecord universe [ir ^clojure.lang.Keyword target-set ^de.prob.statespace.StateSpace statespace variables])

(defn gen-pow-set-vars
  [{:keys [elems]} var-id]
  (vec (map #(keyword (str (name var-id) (name %))) elems)))

(defn generate-var
  [ss ts vars-map var-id]
  (let [t (b/get-type ss var-id)
        ts-string (name (:id ts))]
    (cond
      (= t (str "POW(" ts-string ")")) (assoc vars-map var-id (gen-pow-set-vars ts var-id))
      (= t "INTEGER") vars-map
      (= t "BOOL") vars-map
      :else vars-map)))

(defn generate-variables-map
  [u]
  (let [target-set (util/get-target-set u)
        ss (util/get-statespace u)
        vars (util/get-vars u)
        new-vars-map (reduce (partial generate-var ss target-set) (:variables u) vars)]
    (assoc u :variables new-vars-map)))

(defn unfset-variables
  [^universe u]
  (let [new-vars (util/get-new-var-ids u)
        old-vars (util/get-old-var-ids u)]
    (-> u
        (util/rm-vars old-vars)
        (util/add-vars new-vars))))

;; TODO Maybe use the ProB api to ask which configurations are valid?
(defn unfset-invariant
  [^universe u]
  (let [invariant (util/get-invariant u)]
    (if invariant
      (util/set-invariant u {:tag :invariants
                              :predicate (transform/predicate u (:predicate invariant))})
      u)))

(defn- unroll-operation
  "Takes an operation and returns a list of new unrolled operations."
  [u op]
  (let [{:keys [return name parameters body]} op
        vars (b/get-parameter-vars u parameters)]
    (map (fn [var-id] {:tag :operation
                       :return return
                       :name (keyword (str name (clojure.core/name var-id)))
                       :parameters []
                       :body (transform/body u body)})
         vars)))

(defn unfset-operations
  [^universe u]
  (let [old-ops (util/get-operations u)]
    (util/set-operations u {:tag :operations
                            :operations (mapcat (partial unroll-operation u) old-ops)})))

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
  [ir set-to-rewrite _ d-set-size]
  (if (util/is-deferred? ir set-to-rewrite)
    (map (fn [i] (keyword (str (name set-to-rewrite) i))) (range d-set-size))
    (map keyword (b/set-elems (b/get-statespace ir) set-to-rewrite))))

;; TODO Only make one statespace call instead of 2
(defn unfset
  "The entry Function that does the transformation."
  [max-size deferred-set-size debug ir set-to-rewrite] ;; Parameter order is chosen to make it easy to partial away the config stuff.
  (let [u (->universe ir {:id set-to-rewrite :elems (determine-set-elems ir set-to-rewrite max-size deferred-set-size)} (b/get-statespace ir) {})]
    (-> u
     (generate-variables-map)
     (validate debug)
     (unfset-variables)
     (unfset-invariant)
     (unfset-operations)
     (util/clear-empty-sets)
     (validate debug))))
