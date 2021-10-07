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

(spec/def :fset/deferred-sset-size number?)
(spec/def :fset/set-to-rewrite keyword?)
(spec/def :fset/variables (spec/map-of keyword? set?))

(spec/def :fset/universe (spec/keys :req-un [:lisb/ir
                                             :fset/set-to-rewrite
                                             :fset/deferred-set-size
                                             :fset/variables]))
