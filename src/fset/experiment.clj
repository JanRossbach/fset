(ns fset.experiment
  (:require
   [fset.config :as cfg]
   [clojure.java.io :as io]
   [fset.util :as util]
   [fset.core :as fset]
   [lisb.translation.util :refer [b->lisb lisb->b lisb->ir ir->b]]))

;; Namespace to run the core functions in a repl and experiment with results.

(defn file->lisb!
  [filename]
  (->> filename
       (slurp)
       (read-string)))

(def scheduler (file->lisb! (str cfg/lisb-source-dir "scheduler.edn")))

(def scheduler-ir (lisb->ir scheduler))

(def scheduler-mch (fset/make-mch! (read-string (slurp "resources/machines/lisb/source/scheduler.edn")) cfg/meta-data))

(def lift (file->lisb! (str cfg/lisb-source-dir "Lift.edn")))

(defn transform-b-machines!
  ([]
   (let [machines (.list (io/file cfg/b-source-dir))]
     (transform-b-machines! machines)))
  ([machines]
   (->> machines
        (map #(str cfg/b-source-dir %))
        (map slurp)
        (map b->lisb)
        (map (partial fset/transform cfg/meta-data)))))

(defn print-transform!
  "Takes lisb code and pprints it's B code and transformed B-Code."
  [lisb]
  (println "--------------------IN")
  (println (lisb->b lisb))
  (println "------------------>>>>>")
  (println (fset/transform cfg/meta-data scheduler))
  (println "-------------------OUT"))

(defn save-b!
  [fn content]
  (spit (str cfg/b-target-dir cfg/prefix fn) content))

;; repl with example of executing the high level commands
(comment
  (print-transform! scheduler)

  (print-transform! lift)

  (transform-b-machines!)

  (save-b! "scheduler.mch" scheduler-ir)

  (clojure.pprint/pprint scheduler-ir)

  (ir->b (util/clear-sets scheduler-ir))

  (util/clear-sets {:clauses '({:tag :sets
                                :identifiers (:PID)}
                               {:tag variables
                                :identifiers (:hello)})})

  )
