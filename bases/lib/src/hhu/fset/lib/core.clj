(ns hhu.fset.lib.core
  (:require
   [hhu.fset.encoder.interface :as encoder]
   [hhu.fset.backend.interface :as backend]))

(def default-config
  {:max-unroll-size 200
   :unroll-invariant true
   :unroll-sub true
   :deff-set-size 2
   :logging false
   :prob-logging false
   :excluded-vars #{}})

(def unroll-ops-default-config
  {:max-unroll-size 200
   :unroll-invariant false
   :unroll-sub false
   :deff-set-size 2
   :logging false
   :prob-logging false
   :excluded-vars :all})

(defonce config (atom default-config))

(defn set-config! [new-cfg]
  (reset! config new-cfg))

(defn set-config-var! [key val]
  (swap! config assoc key val))

(defn get-config []
  @config)

(defn reset-config []
  (reset! config default-config))

(defn make-config
  [config kwargs]
  (reduce (fn [cfg [k v]] (assoc cfg k v)) config (partition 2 kwargs)))

(defn boolencode
  [ir & kwargs]
  (encoder/boolencode ir (make-config @config kwargs)))


(defn unroll-ops [ir & kwargs]
  (encoder/boolencode ir (make-config unroll-ops-default-config kwargs)))

(defn num-vars [ir & kwargs]
  (backend/setup-backend ir (make-config @config kwargs))
  (backend/num-vars))

(defn num-ops [ir & kwargs]
  (backend/setup-backend ir (make-config @config kwargs))
  (backend/num-ops))

(defn num-unrollable-vars
  [ir & kwargs]
  (backend/setup-backend ir (make-config @config kwargs))
  (backend/num-unrollable-vars))

(defn num-unrollable-ops
  [ir & kwargs]
  (backend/setup-backend ir (make-config @config kwargs))
  (backend/num-unrollable-ops))
