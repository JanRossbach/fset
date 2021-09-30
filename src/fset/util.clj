(ns fset.util
  (:require
   [com.rpl.specter :as s]))

;; Namespace to provide specter functionality to the transform ns.

(def CLAUSES (s/path [:clauses]))
(def VARIABLES (s/path [CLAUSES #(= (:tag %) :variables) :identifiers]))
(def SETS (s/path [CLAUSES #(= (:tag %) :sets) :set-definitions s/ALL]))
(def INIT (s/path [CLAUSES #(= (:tag %) :init)]))

(defn add-clause
  [ir new-clause]
  (s/setval [CLAUSES s/BEFORE-ELEM] new-clause ir))

(defn add-clause-after
  [ir new-clause]
  (s/setval [CLAUSES s/AFTER-ELEM] new-clause ir))

(defn get-init
  [ir]
  (first (s/select [INIT] ir)))

(defn- make-init-ir
  [[s t]]
  {:tag :assign
   :identifiers (list s)
   :values (list t)})

;; TODO Refactor to just add one init with AFTER-ELEM Navigator
(defn add-inits
  "Takes an lisb IR and a seq that contains vector pairs of
  identifiers and values to be added to the init clause."
  [ir inits]
  (let [new-assigns (map make-init-ir inits)
        prev-init (get-init ir)]
    (cond
      (nil? prev-init) (add-clause ir {:tag :init,
                                       :subsitution {:tag :parallel-substitution
                                                     :substitutions (list new-assigns)}})
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

(defn rm-init-by-id
  [ir id]
  ir)

(defn rm-invar-by-id
  [ir id]
  ir)
