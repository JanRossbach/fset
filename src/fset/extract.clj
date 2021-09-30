(ns fset.extract
  (:require
   [com.rpl.specter :as s]
   [fset.util :as util]))



(defn extract-fun
  "Extracts the variables from a single function. Returns the changed IR"
  [ir f]
  (let [rules (first (s/select [(s/walker #(= (:tag %) :assign))
                                #(= (:identifiers %) (list f))
                                :values
                                s/FIRST] ir))
        vars (map first rules)]
    (-> ir
        (util/set-vars vars)
        (util/add-inits rules)
        (util/clear-sets)
        (util/rm-init-by-id f)
        (util/rm-invar-by-id f))))


(defn extract-vars
  "Takes an IR and a seq of function identifiers and extracts the function
  rules into separate variables."
  [ir fs]
  (if (or (not (seq? fs)) (empty? fs))
    ir
    (loop [r ir
           f (first fs)
           s fs]
      (if (empty? s)
        r
        (recur (extract-fun r f) (first s) (rest s))))))
