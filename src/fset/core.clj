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
(spec/def :lisb/name keyword?)

(spec/def :lisb/ir (spec/keys :req-un [:lisb/tag :lisb/clauses :lisb/name]))

;; fset specs

(spec/def :fset/id keyword?)
(spec/def :fset/elems (spec/coll-of :fset/id))
(spec/def :fset/target-set (spec/keys :req-un [:fset/id :fset/elems]))
(spec/def :fset/target-sets (spec/coll-of :fset/target-set))
(spec/def :fset/set keyword?)
(spec/def :fset/variable (spec/keys :req-un [:fset/id :fset/elems :fset/set]))
(spec/def :fset/variables (spec/coll-of :fset/variable))
(spec/def :lisb/statespace #(= (type %) de.prob.statespace.StateSpace))

(spec/def :fset/universe (spec/keys :req-un [:lisb/ir
                                             :lisb/statespace
                                             :fset/target-sets
                                             :fset/variables]))

(defrecord universe [ir target-sets statespace variables])


(defn unfset-variables
  [^universe u]
  (let [new-vars (util/get-new-var-ids u)
        old-vars (util/get-old-var-ids u)]
    (-> u
        (util/rm-vars old-vars)
        (util/add-vars new-vars))))

(defn unfset-invariant
  [^universe u]
  (let [invariant (util/get-invariant u)]
    (if invariant
      (util/set-invariant u {:tag :invariants
                             :values (map (partial transform/predicate u) (:values invariant))})
      u)))

(defn unfset-initialisation
  [^universe u]
  (let [init (util/get-init u)]
    (if init
      (util/set-init u {:tag :init
                        :values (map (partial transform/substitution u) (:values init))})
      u)))

(defn- unroll-operation
  "Takes an operation and returns a list of new unrolled operations."
  [u op]
  (let [{:keys [return name parameters body]} op
        vars (b/get-parameter-vars u parameters)]
    (map (fn [var-id] {:tag :operation
                       :return return
                       :name (keyword (str (clojure.core/name name) (clojure.core/name var-id)))
                       :parameters []
                       :body (transform/body u body)})
         vars)))

(defn unfset-operations
  [^universe u]
  (let [old-ops (util/get-operations u)]
    (util/set-operations u {:tag :operations
                            :values (mapcat (partial unroll-operation u) old-ops)})))

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

(defn- gen-pow-set-vars
  [ss set-id var-id]
  (vec (map #(keyword (str (name var-id) (name %))) (b/set-elems ss set-id))))

(defn- extract-ts-from-string
  [type-string]
  (if (re-matches #"POW\((.)*\)" type-string)
    (keyword (subs type-string 4 (dec (count type-string))))
    nil))

(defn api-str->keyword
  [var api-result]
  (into #{}
        (for [r api-result]
          (into #{} (map (fn [s] (keyword (str (name var) s)))  r)))))

(defn- generate-var
  [ss vars invar var-id]
  (let [t (b/get-type ss var-id)
        ts (extract-ts-from-string t)
        states (b/get-possible-var-states ss (list var-id) (filter #(not= % var-id) vars) invar)]
    (if (re-matches #"POW\((.)*\)" t)
      {:id var-id
       :set {:tag :power-set :set ts}
       :elems (gen-pow-set-vars ss ts var-id)
       :states (api-str->keyword var-id states)}
      {})))

(defn- generate-variables-map
  [ir ss]
  (let [vars (util/get-vars ir)
        invar (util/get-invariant-as-pred ir)]
    (filter #(not= % {}) (map (partial generate-var ss vars invar) vars))))

(defn- determine-set-elems
  [ir set-to-rewrite d-set-size ss]
  (if (util/is-deferred? ir set-to-rewrite)
    (map (fn [i] (keyword (str (name set-to-rewrite) i))) (range d-set-size))
    (map keyword (b/set-elems ss set-to-rewrite))))

(defn- determine-target-sets
  [ir ss m dss]
  (let [candidate-sets (util/get-set-ids ir)]
    (->> candidate-sets
         (map (fn [id] {:id id
                       :elems (determine-set-elems ir id dss ss)}))
         (filter #(< (count (:elems %)) m))
         vec)))

(defn boolencode
  "The entry Function that does the transformation."
  [max-size deferred-set-size debug ir _] ;; Parameter order is chosen to make it easy to partial away the config stuff.
  (let [ss (b/get-statespace ir)
        target-sets (determine-target-sets ir ss max-size deferred-set-size)
        variables-map (generate-variables-map ir ss)]
    (-> (->universe ir target-sets ss variables-map)
        (validate debug)
        (unfset-variables)
        (unfset-invariant)
        (unfset-initialisation)
        (unfset-operations)
        (util/clear-empty-sets)
        (validate debug))))
