(ns fset.util
  [com.rpl.specter :as s])

(defn get-node-by-tag
  [el-tag ir]
  (s/select [(s/walker #(= (:tag %) el-tag))] ir))

(defn update-node-by-tag
  [el-tag update-fn m]
  (s/transform [:ir (s/walker #(= (:tag %) el-tag))] update-fn m))
