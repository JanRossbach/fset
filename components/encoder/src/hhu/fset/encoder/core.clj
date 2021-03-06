(ns hhu.fset.encoder.core
  (:require
   [clojure.walk :refer [postwalk]]
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
             (unroll-sub true)
             simplify-all)})

(defn unroll-operation
  [op]
  (log/info (str "Unrolling Operation " (:name op)))
  (try (let [bindings (b/op->bindings op)]
         (if (and (seq bindings) (empty? (:returns op)))
           (map (partial new-op op) bindings)
           (list (assoc op :body (unroll-sub (:body op) false)))))
       (catch Exception e
         (log/info (str "Failed translating Operation: " op) e)
         (list (assoc op :body (unroll-sub (:body op) false))))))

(defn unroll-clause
  [c]
  (match c
    {:tag :variables :values v} {:tag :variables :values (map :name (mapcat b/unroll-variable v))}
    {:tag :invariants :values v} {:tag :invariants :values (filter (fn [invar] (not (= {} invar)))
                                                                   (cons (BOOLDEFS (map :name (b/get-all-bools)))
                                                                         (if (b/unroll-invariant?) (map (comp simplify-all unroll-predicate) v) (boolvars->set v))))}
    {:tag :init :values v} (simplify-all {:tag :init :values (map (fn [sub] (unroll-sub sub false)) v)})
    {:tag :operations :values v} {:tag :operations :values (mapcat unroll-operation v)}
    {:tag :assertions :values v} (simplify-all {:tag :assertions :values (map unroll-predicate v)})
    _ c))

(defn unroll-machine
  [{:keys [name machine-clauses]}]
  (MACHINE name (doall (map unroll-clause machine-clauses))))

(defn boolencode
  [ir config]
  (log/swap-config! assoc :min-level (if (:prob-logging config)
                                       :debug
                                       (if (:logging config)
                                         :info
                                         :warn)))
  (try
    (postwalk identity (unroll-machine (b/setup-backend ir config)))
    (catch Exception e
      (log/info (str "Failed Translation of Machine:" ir) e)
      ir)))
