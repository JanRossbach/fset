(ns hhu.fset.backend.util)

(defn create-boolname [& ids]
  (keyword (apply str (map (fn [id] (if (keyword? id) (name id) id)) (flatten ids)))))
