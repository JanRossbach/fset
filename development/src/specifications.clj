(ns specifications
  (:require
   [taoensso.timbre :as log]
   [clojure.string :refer [replace-first]]
   [lisb.translation.util :refer [ir->b b->ir]]
   [hhu.fset.lib.core :as fset]))

(def SRC-DIR (java.io.File. "/home/jan/School/Projektarbeit/translations/src/"))
(def AUTO-DIR (java.io.File. "/home/jan/School/Projektarbeit/translations/auto/"))

(def machine-files (remove (fn [file] (.isDirectory file)) (file-seq SRC-DIR)))

(defn add-auto
  "Adds _auto to a filename before the Extension"
  [filename]
  (replace-first filename #"[.][^.]+$" "_auto.mch"))

(defn translate-machine
  [file]
  (let [filename (.getName file)]
    (try (spit (str AUTO-DIR "/" (add-auto filename))
               (->> file
                    slurp
                    b->ir
                    fset/boolencode
                    ir->b))
         (catch Exception e
           (log/error (str "Failed to translate Machine: " filename) e)))))

(defn translate-machines
  [files]
  (for [file files]
    (translate-machine file)))

(defn clear-directory [directory-path]
  (let [files (remove (fn [file] (.isDirectory file)) (file-seq directory-path))]
    (for [file files]
      (clojure.java.io/delete-file file))))

(defn file-index->ir
  [index]
  (b->ir (slurp (nth machine-files index))))

(comment
  (fset/set-config-var! :logging false)

  (clear-directory SRC-DIR)

  (clear-directory AUTO-DIR)

  (ir->b (fset/boolencode (file-index->ir 4)))

  (translate-machine (nth machine-files 4))

  (nth machine-files 2)

  (translate-machines machine-files)

)
