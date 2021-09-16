(ns fset.transform
  (:require
   [com.rpl.specter :as s]
   [lisb.core :refer [eval-ir-formula]]))

(defn get-node-by-tag
  [el-tag ir]
  (s/select [(s/walker #(= (:tag %) el-tag))] ir))

(defn update-node-by-tag
  [el-tag update-fn m]
  (s/transform [:ir (s/walker #(= (:tag %) el-tag))] update-fn m))

(defn transform-sets
  [m]
  m)

(defn transform-definitions
  [m]
  m)

(defn transform-invariant
  [m]
  m)

(defn transform-operations
  [m]
  m)

(defn transform
  [m]
  (->> m
       (transform-sets)
       (transform-definitions)
       (transform-invariant)
       (transform-operations)))

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
