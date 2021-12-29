(ns hhu.fset.encoder.interface
  (:require [hhu.fset.encoder.core :as core]))

(defn boolencode
  [ir config]
  (core/boolencode ir config))
