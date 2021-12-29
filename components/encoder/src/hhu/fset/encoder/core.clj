(ns hhu.fset.encoder.core
  (:require
   [hhu.fset.dsl.interface :refer [MACHINE AND BOOL BOOLDEFS ASSIGN AND IN]]
   [hhu.fset.encoder.expressions :refer [unroll-expression boolvars->set]]
   [clojure.core.match :refer [match]]
   [taoensso.timbre :as log]
   [hhu.fset.encoder.predicates :refer [unroll-predicate]]
   [hhu.fset.simplifier.interface :refer [simplify-all]]
   [hhu.fset.backend.interface :as b]))

(defn unroll-id-val
  [[id val]]
  (if (b/unrollable-var? id)
    (let [uvar (b/unroll-variable id)]
      (try (let [uexpr (unroll-expression val)]
             (map (fn [v p] (ASSIGN v (BOOL p)))
                  (map :name uvar)
                  uexpr))
           (catch Exception e
             (log/info "Context Assignment " e)
             (map (fn [v p] (ASSIGN v (BOOL p)))
                  (map :name uvar)
                  (map (fn [v] (IN (:elem v) (boolvars->set val))) uvar)))))
    (list (ASSIGN id (boolvars->set val)))))


(defn unroll-sub
  [sub]
  ((fn T [e]
     (match e
       {:tag :parallel-sub :subs substitutions} {:tag :parallel-sub :subs (map T substitutions)}
       {:tag :if-sub :cond condition :then then :else else} {:tag :if-sub :cond (unroll-predicate condition) :then (T then) :else (T else)}
       {:tag :select :clauses clauses} {:tag :select :clauses (mapcat (fn [[P S]] [(unroll-predicate P) (T S)]) (partition 2 clauses))}
       {:tag :any :ids _ :pred _ :subs then} {:tag :parallel-sub :subs (map T then)}
       {:tag :assignment :id-vals id-vals} {:tag :parallel-sub :subs (mapcat unroll-id-val (partition 2 id-vals))}
       _ e))
   sub))

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
             unroll-sub)})

(defn unroll-operation
  [op]
  (log/info (str "Unrolling Operation " (:name op)))
  (let [bindings (b/op->bindings op)]
    (if (seq bindings)
      (map (partial new-op op) bindings)
      (list (assoc op :body (unroll-sub (:body op)))))))

(defn unroll-clause
  [c]
  (match c
    {:tag :variables :values v} {:tag :variables :values (map :name (mapcat b/unroll-variable v))}
    {:tag :invariants :values v} {:tag :invariants :values (filter #(not= % {}) (cons (BOOLDEFS (map :name (b/get-all-bools)))
                                                                                      (if true (map unroll-predicate v) (boolvars->set v))))}
    {:tag :init :values v} {:tag :init :values (map unroll-sub v)}
    {:tag :operations :values v} {:tag :operations :values (mapcat unroll-operation v)}
    {:tag :assertions :values v} {:tag :assertions :values (boolvars->set v)}
    _ c))

(defn unroll-machine
  [{:keys [name clauses]}]
  (MACHINE name (map unroll-clause clauses)))

(defn boolencode
  [ir config]
  (log/swap-config! assoc :min-level (if (:logging config)
                                            :debug
                                            :warn))
  (->> (b/setup-backend ir config)
       (unroll-machine)
       (simplify-all)))
