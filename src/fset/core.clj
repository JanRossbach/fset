(ns fset.core
  (:require
   [clojure.java.io :as io]
   [clojure.pprint :refer [pprint]]
   [fset.config :as cfg]
   [lisb.core :refer [eval-ir-formula]]
   [lisb.translation.util :refer [b->ir ir->ast ir->b lisb->ir]]
   [lisb.prob.animator :refer [state-space!]]
   [lisb.fset.transform :refer [transform]]))

;; Namespace for all the impure io functions.

;; FIXME
(defn get-set-elements
  [set-identifier machine]
  (let [{:keys [ir ss meta]} machine]
    (eval-ir-formula (lisb->ir `(bcomp-set [:x] (bmember? :x ~set-identifier))))))

(defn add-meta-data [m]
  (assoc m :meta (-> cfg/meta-data
                      (assoc :sets-to-transform '(:PID)))))

(defn make-mch!
  ([ir]
   (add-meta-data {:ir ir
                   :ss (state-space! (ir->ast ir))})))

(defn load-mch!
  ([filename]
   (let [input-string (slurp filename)
         ir (b->ir input-string)]
     (make-mch! ir))))

(defn save-mch!
  [ir target-filename]
  (spit target-filename (ir->b ir)))

(defn load-transform-machine!
  [source-filename]
  (transform (load-mch! source-filename)))

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
        m (make-mch! ir)
        transformed-machine (:ir (transform m))]
    (pprint "--------------------------")
    (pprint ir)
    (pprint "--------------------->>>>>")
    (pprint transformed-machine)
    (pprint "--------------------------")))
