(ns fset.extract
  (:require
   [fset.util :as util]))

(defn- extract-fun
  "Extracts the variables from a single function. Returns the changed IR"
  [ir f]
  (let [assign (first (util/get-assigns-by-id ir f))
        rules (apply vec (:values assign))
        vars (map first rules)]
    (-> ir
        (util/add-vars vars)
        (util/add-inits rules)
        (util/rm-var-by-id f)
        (util/rm-init-by-id f)
        (util/rm-typedef-by-id f)
        (util/replace-calls-by-arg f))))


(defn extract
  [ir & fs]
  (reduce extract-fun ir fs))
