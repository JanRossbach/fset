(ns fset.spec
  (:require
   [clojure.spec.alpha :as spec]))

;; lisb specs


(spec/def :lisb/tag keyword?)
(spec/def :lisb/name keyword?)
(spec/def :lisb/parameters vector?)

(spec/def :lisb/variant (spec/keys :req-un [:lisb/tag]))
(spec/def :lisb/header (spec/keys :req-un [:lisb/tag :lisb/name :lisb/parameters]))
(spec/def :lisb/clause map?)
(spec/def :lisb/clauses (spec/or :nil nil?
                                 :clause (spec/coll-of :lisb/clause)))

(spec/def :lisb/ir (spec/keys :req-un [:lisb/tag :lisb/variant :lisb/header :lisb/clauses]))

;; fset specs

(spec/def :fset/deferred-size number?)
(spec/def :fset/max-transform-size number?)
(spec/def :fset/sets-to-transform (spec/coll-of keyword?))
(spec/def :fset/universe (spec/coll-of :fset/atom))

(spec/def :fset/meta (spec/keys :req-un [:fset/deferred-size :fset/max-transform-size]
                                :opt-un [:fset/sets-to-transform :fset/universe]))