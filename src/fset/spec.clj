(ns fset.spec
  (:require
   [clojure.spec.alpha :as spec]))

;; Lisb IR spec

(spec/def :lisb/ir map?)


;; Fset specs

(spec/def :fset/deferred-size number?)
(spec/def :fset/max-transform-size number?)
(spec/def :fset/sets-to-transform (spec/coll-of keyword?))

(spec/def :fset/meta (spec/keys :req-un [:fset/deferred-size :fset/max-transform-size]
                                :opt-un [:fset/sets-to-transform]))
(spec/def :lisb/ss #(= (type %) de.prob.statespace.StateSpace))
(spec/def :fset/mch (spec/keys :req-un [:lisb/ir :lisb/ss :fset/meta]))
