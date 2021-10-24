(ns fset.specs
  (:require
   [clojure.spec.alpha :as s]))



;; Uncomplete spec of the lisb IR

(s/def :lisb/tag keyword?)
(s/def :lisb/name keyword?)
(s/def :lisb/parameters vector?)

(s/def :lisb/variant (s/keys :req-un [:lisb/tag]))
(s/def :lisb/header (s/keys :req-un [:lisb/tag :lisb/name :lisb/parameters]))
(s/def :lisb/predicate map?)
(s/def :lisb/invariant (s/keys :req-un [:lisb/tag :lisb/predicate]))
(s/def :lisb/properties map?)
(s/def :lisb.operation/operation map?)
(s/def :lisb.operation/operations (s/coll-of :lisb.operation/operation))
(s/def :lisb/operations (s/keys :req-un [:lisb/tag :lisb.operation/operations]))
(s/def :lisb/constants map?)
(s/def :lisb/sets map?)
(s/def :lisb/constraints map?)
(s/def :lisb/clause (s/or :invariant :lisb/invariant
                                :sets :lisb/sets
                                :properties :lisb/properties
                                :constants :lisb/constants
                                :operations :lisb/operations))
(s/def :lisb/clauses (s/or :nil nil?
                                 :clause (s/coll-of :lisb/clause)))
(s/def :lisb/name keyword?)

(s/def :lisb/ir (s/keys :req-un [:lisb/tag :lisb/clauses :lisb/name]))

(s/def :lisb/statespace #(= (type %) de.prob.statespace.StateSpace))

;; varencode


(s/def :fset.varencode/id keyword?)
(s/def :fset.varencode/type string?)
(s/def :fset.varencode/base-set keyword?)
(s/def :fset.varencode/size int?)
(s/def :fset.varencode/bool map?)
(s/def :fset.varencode/bools (s/coll-of :fset.varencode/bool))


(s/def :fset.varencode/variable (s/keys :req-un [:fset.varencode/id
                                                 :fset.varencode/type
                                                 :fset.varencode/base-set
                                                 :fset.varencode/size
                                                 :fset.varencode/bools]))




(s/def :fset/universe (s/keys :req-un [:lisb/ir
                                       :lisb/statespace
                                       :fset.varencode/variable]))

;; unrolling


;; function extraction
