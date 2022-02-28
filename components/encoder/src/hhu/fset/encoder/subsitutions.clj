(ns hhu.fset.encoder.subsitutions
  (:require
   [clojure.core.match :refer [match]]
   [hhu.fset.dsl.interface :refer [ASSIGN BOOL IN]]
   [taoensso.timbre :as log]
   [hhu.fset.backend.interface :as b]
   [hhu.fset.encoder.expressions :refer [unroll-expression boolvars->set]]
   [hhu.fset.encoder.predicates :refer [unroll-predicate]]))

(defn unpack-fn-override
  [idval]
  (match idval
         [{:tag :fn-call :f (fid :guard b/unrollable-var?) :args (x :guard b/set-element?)} (val :guard b/set-element?)]
         [fid {:tag :override :rels (list fid #{{:tag :maplet :left x :right val}})}]
    _ idval))

(defn unroll-id-val
  [idval]
  (let [[id val] (unpack-fn-override idval)]
    (if (b/unrollable-var? id)
      (let [uvar (b/unroll-variable id)]
        (try (let [uexpr (unroll-expression val)]
               (map (fn [v p] (ASSIGN v (BOOL p)))
                    (map :name uvar)
                    uexpr))
             (catch Exception e
               (log/info (str "Failed at Assignment: " id val) e)
               (map (fn [v p] (ASSIGN v (BOOL p)))
                    (map :name uvar)
                    (map (fn [v] (IN (:elem v) (boolvars->set val))) uvar)))))
      (list (ASSIGN id (boolvars->set val))))))

(defn unroll-becomes
  [id pred]
  (if (b/unrollable-var? id)
    (let [uvar (b/unroll-variable id)
          upred (unroll-predicate pred)]
      (map (fn [v] {:tag :becomes-such :ids (list v) :pred upred})
           (map :name uvar)))
    (list {:tag :becomes-such :ids (list id) :pred (boolvars->set pred)})))

(defn unroll-sub
  [sub]
  (if (not (b/unroll-sub?))
    sub
    ((fn T [e]
       (match e
         {:tag :parallel-sub :subs substitutions} {:tag :parallel-sub :subs (map T substitutions)}
         {:tag :sequential-substitution :subs substitutions} {:tag :sequential-substitution :subs (map T substitutions)}
         {:tag :let-sub :id-vals id-vals :subs subs} {:tag :parallel-sub :subs (map (fn [sub] (T (b/apply-binding sub (partition 2 id-vals)))) subs)}
         {:tag :precondition :pred pred :subs substitutions} {:tag :precondition :pred (unroll-predicate pred) :subs (map T substitutions)}
         {:tag :assert :pred pred :subs substitutions} {:tag :assert :pred (unroll-predicate pred) :subs (map T substitutions)}
         {:tag :choice :subs substitutions} {:tag :choice :subs (map T substitutions)}
         {:tag :if-sub :cond condition :then then :else else} {:tag :if-sub :cond (unroll-predicate condition) :then (T then) :else (T else)}
         {:tag :select :clauses clauses} {:tag :select :clauses (mapcat (fn [[P S]] [(unroll-predicate P) (T S)]) (partition 2 clauses))}
         {:tag :any :ids _ :pred _ :subs then} {:tag :parallel-sub :subs (map T then)}
         {:tag :assignment :id-vals id-vals} {:tag :parallel-sub :subs (mapcat unroll-id-val (partition 2 id-vals))}
         {:tag :becomes-element-of :ids ids :set sete} {:tag :becomes-element-of :ids ids :set (boolvars->set sete)}
         {:tag :becomes-such :ids ([id] :seq) :pred pred} {:tag :parallel-sub :subs (unroll-becomes id pred)}
         _ e))
     sub)))
