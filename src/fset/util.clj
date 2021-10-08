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
(def TYPEDEFS (s/path [INVAR :predicate (s/if-path (s/must :predicates) [:predicates s/ALL] s/STAY)]))
(def NEW-VARS (s/path [:variables s/MAP-VALS s/ALL]))
(def OLD-VARS (s/path [:variables s/MAP-KEYS]))

(defn get-new-var-ids
  [u]
  (s/select [NEW-VARS] u))

(defn get-old-var-ids
  [u]
  (s/select [OLD-VARS] u))

(defn get-init
  [u]
  (first (s/select [INIT] u)))

(defn add-clause
  [u new-clause]
  (s/setval [CLAUSES s/BEFORE-ELEM] new-clause u))

(defn add-clause-after
  [u new-clause]
  (s/setval [CLAUSES s/AFTER-ELEM] new-clause u))

(defn get-vars
  [u]
  (s/select [VARIABLES s/ALL] u))

(defn set-vars
  [u vars]
  (s/setval [VARIABLES] vars u))

(defn add-vars
  [u vars]
  (let [old-vars (get-vars u)]
    (if (empty? old-vars)
      (add-clause u {:tag :variables :identifiers vars})
      (s/setval [VARIABLES] (concat old-vars vars) u))))

(defn rm-var-by-id
  [u id]
  (if (= 1 (count (get-vars u)))
    (s/setval [(CLAUSE :variables)] s/NONE u)
    (s/setval [VARIABLES s/ALL #(= id %)] s/NONE u)))

(defn rm-vars
  [u ids]
  (reduce rm-var-by-id u ids))

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
  [u ids]
  (reduce rm-init-by-id u ids))

(defn get-invariant
  [u]
  (first (s/select [INVAR] u)))

(defn set-invariant
  [u new-invar]
  (if (nil? (get-invariant u))
    (add-clause u new-invar)
    (s/setval [INVAR] new-invar u)))

(defn rm-typedef-by-id
  [u id]
  (s/setval [TYPEDEFS #(= (:element %) id)] s/NONE u))

(defn replace-calls-by-arg
  [u id]
  (s/transform [(s/walker #(= (:tag %) :call)) #(= (:f %) id)] #(first (:args %)) u))

(defn get-type
  [u var]
  (s/select [TYPEDEFS (TAG :member) #(= (:element %) var) :set] u))

(defn get-properties
  [u]
  (s/select [PROPERTIES] u))

(defn set-properties
  [u new-properties]
  (if (nil? (get-properties u))
    (add-clause u new-properties)
    (s/setval [PROPERTIES] new-properties u)))

(defn add-typedef
  [u [v s]]
  (s/setval [TYPEDEFS s/ALL s/AFTER-ELEM]
            {:tag :member
                :element v
                :set s}
            u))

(defn add-typedefs
  [u defs]
  (reduce add-typedef u defs))
