(ns fset.encoder.interface
  (:require [fset.encoder.core :as core]))

(defn boolencode
  [ir config]
  (core/boolencode ir config))
