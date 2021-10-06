(ns fset.util
  (:require
   [com.rpl.specter :as s]))

;; Namespace to provide specter functionality to the transform ns.

(defn TAG [t] (s/path #(= (:tag %) t)))
(def CLAUSES (s/if-path (s/must :ir) [:ir :clauses] [:clauses]))
(defn CLAUSE [^clojure.lang.Keyword name] (s/path [CLAUSES s/ALL (TAG name)]))
(def VARIABLES (s/path [(CLAUSE :variables) :identifiers]))
(def SETS (s/path [(CLAUSE :sets) :set-definitions s/ALL]))
(def INIT (s/path [(CLAUSE :init)]))
(def INVAR (s/path [(CLAUSE :invariants)]))
(def PAR-ASSIGNS (s/path [INIT :substitution :substitutions s/ALL]))
(def PROPERTIES (s/path [(CLAUSE :properties)]))

(defn add-clause
  [ir new-clause]
  (s/setval [CLAUSES s/BEFORE-ELEM] new-clause ir))

(defn add-clause-after
  [ir new-clause]
  (s/setval [CLAUSES s/AFTER-ELEM] new-clause ir))

(defn get-init
  [ir]
  (first (s/select [INIT] ir)))

(defn- make-assign-ir
  [[s t]]
  {:tag :assign
   :identifiers (list s)
   :values (list t)})

(defn add-inits
  "Takes an lisb IR and a seq that contains vector pairs of
  identifiers and values to be added to the init clause."
  [ir inits]
  (let [new-assigns (map make-assign-ir inits)
        prev-init (get-init ir)]
    (cond
      (and (nil? prev-init) (= (count inits) 1)) (add-clause ir {:tag :init
                                                                 :substitution (first new-assigns)})
      (nil? prev-init) (add-clause ir {:tag :init,
                                       :substitution {:tag :parallel-substitution
                                                     :substitutions new-assigns}})
      (= (:tag (:substitution prev-init)) :assign) (s/setval [INIT]
                                                             {:tag :init
                                                              :substitution {:tag :parallel-substitution
                                                                             :substitutions (conj new-assigns (:substitution prev-init))}}
                                                             ir)
      :else (s/transform [INIT :substitution :substitutions]
                         #(concat % new-assigns)
                         ir))))

(defn get-vars
  [ir]
  (s/select [VARIABLES s/ALL] ir))

(defn set-vars
  [ir vars]
  (s/setval [VARIABLES] vars ir))

(defn add-vars
  [ir vars]
  (let [old-vars (get-vars ir)]
    (if (empty? old-vars)
      (add-clause ir {:tag :variables :identifiers vars})
      (s/setval [VARIABLES] (concat old-vars vars) ir))))

(defn get-sets
  [ir]
  (s/select [SETS] ir))

(defn clear-sets
  [ir]
  (s/setval [(s/walker #(= (:tag %) :sets))] s/NONE ir))

(defn get-assigns-by-id
  [ir id]
  (s/select [(s/walker #(= (:tag %) :assign))
             #(= (:identifiers %) (list id))]
            ir))

(defn count-inits
  [ir]
  (let [init-clause (get-init ir)]
    (cond
      (nil? init-clause) 0
      (= :assign (:tag (:substitution init-clause))) 1
      :else (count (:substitutions (:substitution init-clause))))))

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
  [ir ids]
  (reduce rm-init-by-id ir ids))

(defn get-invariant
  [ir]
  (first (s/select [INVAR] ir)))

(defn set-invariant
  [u new-invar]
  (s/setval [INVAR] new-invar u))

(defn rm-typedef-by-id
  [ir id]
  (s/setval [INVAR :predicate :predicates s/ALL #(= (:element %) id)] s/NONE ir))

(defn replace-calls-by-arg
  [ir id]
  (s/transform [(s/walker #(= (:tag %) :call)) #(= (:f %) id)] #(first (:args %)) ir))

(defn rm-var-by-id
  [ir id]
  (s/setval [VARIABLES s/ALL #(= id %)] s/NONE ir))

(defn get-type
  [ir var]
  (s/select [INVAR :predicate :predicates s/ALL (TAG :member) #(= (:element %) var) :set] ir))

(defn generate-variables
  "Statically analyzes the IR and generates a map of bindings from variable id's that need to be rewritten to set of boolean ids corresponding to the variable in the new machine."
  [ir]
  (let [m (s/select [] ir)]
    m))

(defn get-properties
  [u]
  (s/select [PROPERTIES] u))

(defn set-properties
  [u new-properties]
  (s/setval [PROPERTIES] new-properties u))
