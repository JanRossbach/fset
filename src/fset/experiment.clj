(ns fset.experiment
  (:require
   [fset.config :as cfg]
   [clojure.java.io :as io]
   [fset.core :as fset]
   [lisb.translation.util :refer [b->lisb lisb->b]]
   [clojure.string :as string]))

;; Namespace to run the core functions in a repl and experiment with results.

(defn file->lisb!
  [filename]
  (->> filename
       (slurp)
       (read-string)))

(def scheduler (file->lisb! (str cfg/lisb-source-dir "scheduler.edn")))

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
        (map fset/transform))))

(defn printlns [s]
  (map println (string/split s #"\n")))

(defn print-transform!
  "Takes lisb code and pprints it's IR and it transformed IR."
  [lisb]
  (printlns (lisb->b lisb))
  (println "------------------>>>>>")
  (printlns (fset/transform scheduler)))

(defn save-b!
  [fn content]
  (spit (str cfg/b-target-dir cfg/prefix fn) content))


;; repl with example of executing the high level commands
(comment
  (print-transform! scheduler)

  (print-transform! lift)

  (transform-b-machines!)

  (save-b! "scheduler.mch" (lisb->b scheduler))

  )
