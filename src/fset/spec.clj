(ns fset.spec
  (:require
   [clojure.spec.alpha :as spec]))

;; lisb specs


(spec/def :lisb/tag keyword?)
(spec/def :lisb/name keyword?)
(spec/def :lisb/parameters vector?)

(spec/def :lisb/variant (spec/keys :req-un [:lisb/tag]))
(spec/def :lisb/header (spec/keys :req-un [:lisb/tag :lisb/name :lisb/parameters]))
(spec/def :lisb/predicate map?)
(spec/def :lisb/invariant (spec/keys :req-un [:lisb/tag :lisb/predicate]))
(spec/def :lisb/properties map?)
(spec/def :lisb.operation/operation map?)
(spec/def :lisb.operation/operations (spec/coll-of :lisb.operation/operation))
(spec/def :lisb/operations (spec/keys :req-un [:lisb/tag :lisb.operation/operations]))
(spec/def :lisb/constants map?)
(spec/def :lisb/sets map?)
(spec/def :lisb/constraints map?)
(spec/def :lisb/clause (spec/or :invariant :lisb/invariant
                                :sets :lisb/sets
                                :properties :lisb/properties
                                :constants :lisb/constants
                                :operations :lisb/operations))
(spec/def :lisb/clauses (spec/or :nil nil?
                                 :clause (spec/coll-of :lisb/clause)))

(spec/def :lisb/ir (spec/keys :req-un [:lisb/tag :lisb/variant :lisb/header :lisb/clauses]))

;; fset specs

(spec/def :fset/set-size number?)
(spec/def :fset/target-set keyword?)
(spec/def :fset/variables (spec/map-of keyword? (spec/coll-of keyword?)))

(spec/def :fset/universe (spec/keys :req-un [:lisb/ir
                                             :fset/target-set
                                             :fset/set-size
                                             :fset/variables]))
