(ns hhu.fset.lib.core
  (:require
   [hhu.fset.encoder.interface :as encoder]))

(def default-config
  {:max-unroll-size 200
   :unroll-invariant true
   :simplify-result true
   :deff-set-size 2
   :logging false
   :excluded-vars #{}})

(defonce config (atom default-config))

(defn set-config! [new-cfg]
  (reset! config new-cfg))

(defn set-config-var! [key val]
  (swap! config assoc key val))

(defn get-config []
  @config)

(defn reset-config []
  (reset! config default-config))

(defn boolencode [ir & kwargs]
  (encoder/boolencode ir (reduce (fn [cfg [k v]] (assoc cfg k v)) @config (partition 2 kwargs))))
