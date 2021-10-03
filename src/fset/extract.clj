(ns fset.extract
  (:require
   [fset.util :as util]))

(defn- extract-fun
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
  "Extracts the variables from all given functions. Takes an IR and a variable amount of function ids.
  Returns the updated IR with extracted variables."
  [ir & fs]
  (reduce extract-fun ir fs))
