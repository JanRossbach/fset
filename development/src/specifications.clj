(ns specifications
  (:require
   [taoensso.timbre :as log]
   [hhu.fset.backend.lisb-util :refer [model-check]]
   [clojure.string :refer [replace-first]]
   [lisb.translation.util :refer [ir->b b->ir]]
   [hhu.fset.lib.core :as fset]
   [clojure.pprint :refer [pprint]]))

(def SRC-DIR (java.io.File. "/home/jan/School/Projektarbeit/translations/src/"))
(def VIABLE-DIR (java.io.File. "/home/jan/School/Projektarbeit/translations/viable/"))
(def AUTO-DIR (java.io.File. "/home/jan/School/Projektarbeit/translations/auto/"))
(def FERTIG-DIR (java.io.File. "/home/jan/School/Projektarbeit/translations/fertig/"))
(def DEBUG-DIR (java.io.File. "/home/jan/School/Projektarbeit/translations/debug/"))

(def machine-files (remove (fn [file] (.isDirectory file)) (file-seq SRC-DIR)))

(defn add-auto
  "Adds _auto to a filename before the Extension"
  [filename]
  (replace-first filename #"[.][^.]+$" "_auto.mch"))

(defn translate-machine
  [file]
  (log/warn (str "Translating Machine: " (.getName file)))
  (let [filename (.getName file)]
    (try (spit (str AUTO-DIR "/" (add-auto filename))
               (->> file
                    slurp
                    b->ir
                    fset/boolencode
                    ir->b))
         (catch Exception e
           (log/warn (str "Failed to translate Machine: " filename) e) e))))

(defn translate-machines [directory-path]
  (let [files (remove (fn [file] (.isDirectory file)) (file-seq directory-path))]
    (for [file files]
      (translate-machine file))))

(defn clear-directory [directory-path]
  (let [files (remove (fn [file] (.isDirectory file)) (file-seq directory-path))]
    (for [file files]
      (clojure.java.io/delete-file file))))

(defn file-index->ir
  [index]
  (b->ir (slurp (nth machine-files index))))

(defn correct-machine?
  [old-ir new-ir]
  (let [old-mc (model-check old-ir)
        new-mc (model-check new-ir)]
    (= (:transitions old-mc) (:transitions new-mc))))

(defn sort-machines [src-dir]
  (let [src-files (remove (fn [file] (.isDirectory file)) (file-seq src-dir))]
    (for [src-file src-files]
      (do (log/error (str "Sorting Machine: " (.getName src-file)))
          (try
            (let [new-filename (add-auto (.getName src-file))
                  old-ir (b->ir (slurp src-file))
                  new-ir (b->ir (slurp (str AUTO-DIR "/" new-filename)))]
              (if (correct-machine? old-ir new-ir)
                (spit (str FERTIG-DIR "/" new-filename) (ir->b new-ir))
                (spit (str DEBUG-DIR "/" new-filename) (ir->b new-ir))))
            (catch Exception e
              (log/error (str "Failed sorting Machine: " (.getName src-file)) e)))))))

(comment

  (fset/set-config-var! :logging false)

  (def test-ir (b->ir (slurp (str SRC-DIR "/NormalisationTest.mch"))))
  (fset/boolencode test-ir)

  (clear-directory SRC-DIR)
  (clear-directory AUTO-DIR)

  (ir->b (fset/boolencode (file-index->ir 4)))

  (translate-machine (nth machine-files 4))

  (def res (model-check (b->ir (slurp (nth machine-files 4)))))

  (pprint res)

  (translate-machines VIABLE-DIR)

  (sort-machines SRC-DIR)

)
