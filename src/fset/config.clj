(ns fset.config)

;; Setting global configuration constants

;; define working Directories
(def b-source-dir "resources/machines/b/source/")
(def b-target-dir "resources/machines/b/target/")
(def lisb-source-dir "resources/machines/lisb/source/")
(def lisb-target-dir "resources/machines/lisb/target/")

;; Prefix of the resulting files after running transform-save-machines!
(def prefix "rw_") ;

;; Set the metadata for the Translation Process.
(def meta-data {:deferred-size 3
                :max-transform-size 10})

;; Specify a List of Machines if you don't want all of them to transform.
(def machines-to-transform
  '("Lift.mch"
    ; "scheduler.mch"
    ))
