(ns fset.extract
  (:require
   [clojure.core.match :refer [match]]
   [com.rpl.specter :as s]
   [lisb.translation.lisb2ir :refer [bassign bparallel-substitution]]))

(defn get-varnames
  [ir id]
  (s/select [:clauses s/ALL #(= (:tag %) :init) :values s/ALL (s/walker #(= (:identifiers %) (list id))) :values s/ALL s/ALL s/FIRST] ir))

(defn extract-fn
  [id varnames ir]
  (match ir
         {:tag :variables :values v} {:tag :variables :values (concat (filter #(not= id %) v) varnames)}
         {:tag :apply :f (_ :guard #(= % id)) :args ([varname] :seq)} varname
         {:tag :assign
          :identifiers (identifiers :guard #(= % (list id)))
          :values ([S] :seq)} (apply bparallel-substitution (map (fn [[varname val]] (bassign varname val)) S))
         _ nil))

(defn extract
  [ir id]
  (let [varnames (get-varnames ir id)]
    (s/transform [(s/walker (partial extract-fn id varnames))] (partial extract-fn id varnames) ir)))
