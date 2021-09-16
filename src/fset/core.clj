(ns fset.core
  (:require
   [clojure.java.io :as io]
   [clojure.pprint :refer [pprint]]
   [fset.config :as cfg]
   [lisb.translation.util :refer [ast->ir ir->ast ir->b b->ast lisb->ir]]
   [lisb.prob.animator :refer [state-space!]]
   [lisb.fset.transform :refer [transform]]))

;; Namespace for all the impure io functions.

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

(defn load-transform-machine!
  [source-filename]
  (transform (load-mch! source-filename cfg/meta-data)))

(defn load-transform-save-machine!
  [source-filename target-filename]
  (save-mch! (:ir (load-transform-machine! source-filename)) target-filename))

(defn transform-machines!
  "Transforms all or just a list of the B machines in the set source directory and
   returns a list of the IR's"
  ([]
   (let [machines (.list (io/file cfg/b-source-dir))]
     (for [m machines]
       (load-transform-machine! (str cfg/b-source-dir m)))))
  ([machines]
   (for [m machines]
     (load-transform-machine! (str cfg/b-source-dir m)))))

(defn transform-save-machines!
  "Transforms all or just a list of the B machines in the set source directory and
   saves the as files in the set target directory."
  ([]
   (let [machines (.list (io/file cfg/b-source-dir))]
     (for [m machines]
       (load-transform-save-machine! (str cfg/b-source-dir m)
                                     (str cfg/b-target-dir cfg/prefix m)))))
  ([machines]
   (for [m machines]
     (load-transform-save-machine! (str cfg/b-source-dir m)
                                   (str cfg/b-target-dir cfg/prefix m)))))

(defn print-transform!
  "Takes lisb code and pprints it's IR and it transformed IR."
  [lisb]
  (let [ir (lisb->ir lisb)
        m (make-mch! ir cfg/meta-data)
        transformed-machine (:ir (transform m))]
    (pprint "--------------------------")
    (pprint ir)
    (pprint "--------------------->>>>>")
    (pprint transformed-machine)
    (pprint "--------------------------")))
