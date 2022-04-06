(ns demo
  (:require
   [clojure.pprint :refer [pprint]]
   [clojure.string :refer [replace-first]]
   [lisb.translation.util :refer [b->ir ir->b]]
   [hhu.fset.lib.core :as fset]))


(def SRC-DIR "components/encoder/resources/encoder/")

(defn read-machine
  [filename]
  (b->ir (slurp (str SRC-DIR "/" filename))))

(def scheduler-ir (read-machine "scheduler.mch"))
(def train-ir (read-machine "Train.mch"))
(def demo2-ir (read-machine "Demo(2).mch"))

(defn add-auto
  "Adds _auto to a filename before the Extension"
  [filename]
  (replace-first filename #"[.][^.]+$" "_auto.mch"))

(defn translate-machine
  [filename]
  (let [machine-ir (read-machine filename)
        translated-ir (fset/boolencode machine-ir)
        auto-filename (str SRC-DIR "/" (add-auto filename))]
    (spit auto-filename (ir->b translated-ir))))

(comment

  (translate-machine "scheduler.mch")

  (translate-machine "Demo(2).mch")

  (translate-machine "Train.mch")

  )
