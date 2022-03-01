(ns hhu.fset.encoder.core
  (:require
   [hhu.fset.dsl.interface :refer [MACHINE AND BOOLDEFS AND]]
   [hhu.fset.encoder.translations :refer [boolvars->set unroll-sub unroll-predicate]]
   [clojure.core.match :refer [match]]
   [taoensso.timbre :as log]
   [hhu.fset.simplifier.interface :refer [simplify-all]]
   [hhu.fset.backend.interface :as b]))

(defn add-guards
  [op guards]
  (assoc op :body {:tag :select :clauses (list (apply AND guards) (:body op))}))

(defn lift-guards
  [op]
  (let [guards (b/get-non-det-guards op)]
    (if (empty? guards)
      op
      (add-guards op guards))))

(defn new-op
  [old-op binding]
  {:tag :op
   :name (apply b/create-boolname (:name old-op) (map second binding))
   :returns []
   :args []
   :body (-> old-op
             lift-guards
             :body
             (b/apply-binding binding)
             unroll-sub
             simplify-all)})

(defn unroll-operation
  [op]
  (log/info (str "Unrolling Operation " (:name op)))
  (try (let [bindings (b/op->bindings op)]
         (if (seq bindings)
           (map (partial new-op op) bindings)
           (list (assoc op :body (unroll-sub (:body op))))))
       (catch Exception e
         (log/info (str "Failed translating Operation: " op) e)
         (list (boolvars->set op)))))

(defn unroll-clause
  [c]
  (match c
    {:tag :variables :values v} {:tag :variables :values (map :name (mapcat b/unroll-variable v))}
    {:tag :invariants :values v} {:tag :invariants :values (filter #(not= % {}) (cons (BOOLDEFS (map :name (b/get-all-bools)))
                                                                                      (if (b/unroll-invariant?) (map (comp simplify-all unroll-predicate) v) (boolvars->set v))))}
    {:tag :init :values v} (simplify-all {:tag :init :values (map unroll-sub v)})
    {:tag :operations :values v} {:tag :operations :values (mapcat unroll-operation v)}
    {:tag :assertions :values v} {:tag :assertions :values (boolvars->set v)}
    _ c))

(defn unroll-machine
  [{:keys [name machine-clauses]}]
  (MACHINE name (map unroll-clause machine-clauses)))

(defn boolencode
  [ir config]
  (log/swap-config! assoc :min-level (if (:logging config)
                                       :debug
                                       :warn))
  (let [res (unroll-machine (b/setup-backend ir config))]
    (if (:simplify-result config)
      (simplify-all res)
      res)))
