(ns hhu.fset.cli.core
  (:require
   [clojure.java.io :as io]
   [clojure.tools.cli :refer [parse-opts]]
   [hhu.fset.encoder.interface :refer [boolencode]]
   [clojure.string :refer [replace-first]]
   [lisb.translation.util :refer [ir->b b->ir]])
  (:gen-class))

(defn add-auto
  "Adds _auto to a filename before the Extension"
  [filename]
  (replace-first filename #"[.][^.]+$" "_auto.mch"))

(defn translate-machine
  [filename options]
  (let [cfg (assoc options :unroll-invariant true :unroll-sub true :excluded-vars #{})]
    (spit (str (add-auto filename))
          (ir->b (boolencode (b->ir (slurp filename)) cfg)))))

(def cli-options
  [["-d" "--deff-set-size SIZE" "Number of elements defferred Sets are expanded to"
    :default 2
    :parse-fn #(Integer/parseInt %)
    :validate [#(< 1 %) "Must be a number larger than 1"]]
   ;; A non-idempotent option (:default is applied first)
   ["-m" "--max-unroll-size SIZE" "Maximum number of elements that are expanded"
    :default 200
    :parse-fn #(Integer/parseInt %)
    :validate [#(< 1 %) "Must be a number larger than 1"]]
   ["-l" "--logging"]
   ["-p" "--prob-logging"]
   ["-k" "--keep-statespace"]
   ["-h" "--help"]])

(defn -main [& args]
  (let [parse (parse-opts args cli-options)
        options (:options parse)
        arguments (:arguments parse)]
    (if (:help options)
      (println "Arguments should be relative file-paths to the current working dir.\n"
               "Options:\n"
       (:summary parse))
      (do
        (vec (for [arg arguments]
               (translate-machine
                (io/file arg) options)))
        (System/exit 0)))))
