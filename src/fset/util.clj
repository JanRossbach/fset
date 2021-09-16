(ns fset.util
  (:require
   [lisb.translation.util :refer [ast->ir ir->ast ir->b b->ast]]
   [lisb.prob.animator :refer [state-space!]]))

(defn load-mch!
  ([filename]
   (let [input-string (slurp filename)
         ast (b->ast input-string)]
     {:ir (ast->ir ast)
      :ss (state-space! ast)
      :meta {}}))
  ([filename meta-data]
   (let [input-string (slurp filename)
         ast (b->ast input-string)]
     {:ir (ast->ir ast)
      :ss (state-space! ast)
      :meta meta-data})))

(defn make-mch!
  ([ir]
   {:ir ir
    :ss (state-space! (ir->ast ir))
    :meta {}})
  ([ir meta-data]
   {:ir ir
    :ss (state-space! (ir->ast ir))
    :meta meta-data}))

(defn save-mch!
  [ir target-filename]
  (spit target-filename (ir->b ir)))
