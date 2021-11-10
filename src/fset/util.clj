(ns fset.util
  (:require
   [com.rpl.specter :as s]))

;; Namespace to provide specter functionality to the transform ns.

(defn TAG [t] (s/path #(= (:tag %) t)))
(def CLAUSES (s/if-path (s/must :ir) [:ir :clauses] [:clauses]))
(defn CLAUSE [^clojure.lang.Keyword name] (s/path [CLAUSES s/ALL (TAG name)]))
(def VARIABLES (s/path [(CLAUSE :variables) :values]))
(def SETS (s/path [(CLAUSE :sets) :values s/ALL]))
(def INIT (s/path [(CLAUSE :init)]))
(def INVAR (s/path [(CLAUSE :invariants)]))
(def PAR-ASSIGNS (s/path [INIT :substitution :substitutions s/ALL]))
(def PROPERTIES (s/path [(CLAUSE :properties)]))
(def TYPEDEFS (s/path [INVAR :predicate (s/if-path (s/must :predicates) [:predicates] s/STAY)]))
(def OPERATIONS (s/path [(CLAUSE :operations)]))
(def TARGET-SETS (s/path [:target-sets s/ALL]))

;; Predicates

(defn is-deferred?
  [ir ts]
  (let [sets (s/select [SETS #(= (:identifier %) ts)] ir)]
    (if (empty? sets)
      (throw (ex-info "No set definition with this identifier found" {:set-id ts
                                                                      :ir ir}))
      (= (:tag (first sets)) :deferred-set))))

;; GETTERS

(defn get-init
  [u]
  (first (s/select [INIT] u)))

(defn get-vars
  [u]
  (s/select [VARIABLES s/ALL] u))

(defn get-assigns-by-id
  [ir id]
  (s/select [(s/walker #(= (:tag %) :assign))
             #(= (:identifiers %) (list id))]
            ir))

(defn get-invariant
  [u]
  (first (s/select [INVAR] u)))

(defn get-invariants
  [u]
  (s/select [INVAR :values s/ALL] u))

(defn get-invariant-as-pred
  [u]
  {:tag :and :predicates (apply list (s/select [INVAR :values s/ALL] u))})

(defn get-properties
  [u]
  (first (s/select [PROPERTIES] u)))

(defn get-target-sets
  [u]
  (s/select [TARGET-SETS] u))

(defn get-target-set-ids
  [u]
  (s/select [TARGET-SETS :id] u))

(defn get-operations
  [u]
  (s/select [OPERATIONS :values s/ALL] u))

(defn get-op
  [u op-id]
  (first (s/select [OPERATIONS :values s/ALL #(= (:name %) op-id)] u)))

(defn get-sets
  [u]
  (s/select [SETS] u))

(defn get-set-ids
  [u]
  (s/select [SETS :identifier] u))

(defn get-var-elems
  [u var-id]
  (s/select [:variables s/ALL #(= (:id %) var-id) :elems s/ALL] u))

;; ADDING

(defn add-clause
  [u new-clause]
  (s/setval [CLAUSES s/BEFORE-ELEM] new-clause u))

(defn add-clause-after
  [u new-clause]
  (s/setval [CLAUSES s/AFTER-ELEM] new-clause u))

(defn add-vars
  [u vars]
  (if (empty? vars)
    u
    (let [old-vars (get-vars u)]
      (if (empty? old-vars)
        (add-clause u {:tag :variables :values (apply list vars)})
        (s/setval [VARIABLES] (concat old-vars vars) u)))))


(defn add-typedef
  [u [v s]]
  (let [prev-invar (get-invariant u)
        prev-pred (:predicate prev-invar)]
    (if (not= (:tag prev-pred) :and)
      (s/setval [INVAR] {:tag :invariants
                         :predicate {:tag :and
                                     :predicates (list prev-pred {:tag :member
                                                                  :element v
                                                                  :set s})}} u)
      (s/setval [TYPEDEFS s/AFTER-ELEM]
                {:tag :member
                 :element v
                 :set s}
                u))))

(defn add-typedefs
  [u defs]
  (reduce add-typedef u defs))


;; SETTERS


(defn set-vars
  [u vars]
  (s/setval [VARIABLES] vars u))

(defn set-invariant
  [u new-invar]
  (if (nil? (get-invariant u))
    (add-clause u new-invar)
    (s/setval [INVAR] new-invar u)))

(defn set-properties
  [u new-properties]
  (if (nil? (get-properties u))
    (add-clause u new-properties)
    (s/setval [PROPERTIES] new-properties u)))

(defn set-operations
  [u ops]
  (s/setval [OPERATIONS] ops u))

(defn set-init
  [u init]
  (s/setval [INIT] init u))


(defn- make-assign-ir
  [[s t]]
  {:tag :assign
   :identifiers (list s)
   :values (list t)})

(defn add-inits
  "Takes an lisb IR and a seq that contains vector pairs of
  identifiers and values to be added to the init clause."
  [u inits]
  (let [new-assigns (map make-assign-ir inits)
        prev-init (get-init u)]
    (cond
      (and (nil? prev-init) (= (count inits) 1)) (add-clause u {:tag :init
                                                                 :substitution (first new-assigns)})
      (nil? prev-init) (add-clause u {:tag :init,
                                       :substitution {:tag :parallel-substitution
                                                     :substitutions new-assigns}})
      (= (:tag (:substitution prev-init)) :assign) (s/setval [INIT]
                                                             {:tag :init
                                                              :substitution {:tag :parallel-substitution
                                                                             :substitutions (conj new-assigns (:substitution prev-init))}}
                                                             u)
      :else (s/transform [INIT :substitution :substitutions]
                         #(concat % new-assigns)
                         u))))

(defn count-inits
  [u]
  (let [init-sub (first (s/select [INIT :values s/ALL] u))]
    (if init-sub
      (if (:substitutions init-sub)
        (count (:substitutions init-sub))
        1)
      0)))

(defn rm-init-by-id
  [ir id]
  (let [init (get-init ir)
        n (count-inits ir)]
    (cond
      (= n 0) ir
      (and (= n 1) (= id (first (:identifiers (:substitution init))))) (s/setval [INIT] s/NONE ir)
      (= n 1) ir
      (and  (= n 2) (seq (s/select [PAR-ASSIGNS #(= (:identifiers %) (list id))] ir)))
      (s/setval [INIT] {:tag :init
                        :substitution (first (s/select
                                              [PAR-ASSIGNS #(not= (:identifiers %) id)]
                                              ir))} ir)
      (= n 2) ir
      (> n 2) (s/setval [PAR-ASSIGNS #(= (:identifiers %) (list id))] s/NONE ir))))

(defn rm-inits-by-id
  [u ids]
  (reduce rm-init-by-id u ids))

(defn rm-typedef-by-id
  [u id]
  (s/setval [TYPEDEFS #(= (:element %) id)] s/NONE u))

(defn replace-calls-by-arg
  [u id]
  (s/transform [(s/walker #(= (:tag %) :call)) #(= (:f %) id)] #(first (:args %)) u))

(defn get-constants
  [u]
  (s/select [(CLAUSE :constants) :values s/ALL] u))

(defn get-vars-and-constants
  [u]
  (let [vars (get-vars u)
        constants (get-constants u)]
    (concat vars constants)))

(defn get-invariant-and-properties-as-pred
  [u]
  (let [invar-preds (:values (get-invariant u))
        prop-preds (:values (get-properties u))]
    {:tag :and
     :predicates (concat invar-preds prop-preds)}))
