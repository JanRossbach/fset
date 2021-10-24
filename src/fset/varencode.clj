(ns fset.varencode
  (:require
   [fset.backend :as b]
   [fset.util :as u]
   [fset.specs :refer :all]
   [clojure.spec.alpha :as spec]
   [fset.transformations :as transform]))


(defrecord universe [ir statespace variable])

;; (defn encode-invariant
;;   [^universe u]
;;   (let [invariant (u/get-invariant u)]
;;     (if invariant
;;       (u/set-invariant u {:tag :invariants
;;                              :values (map (partial transform/T_set u) (:values invariant))})
;;       u)))

;; (defn encode-initialisation
;;   [^universe u]
;;   (let [init (u/get-init u)]
;;     (if init
;;       (u/set-init u {:tag :init
;;                         :values (map (partial transform/substitution u) (:values init))})
;;       u)))

;; FIXME
(defn valid-var?
  [ir var-id]
  true)

;; FIXME
(defn set-id
  [s]
  :PID)

;; FIXME
(defn calc-bools
  [typ ms dss var-id set-id]
  '({:name "activePID1" :var :active :element :PID1 :set :PID} {:name "activePID2"}))

(defn validate
  "Debugging helper function using clojure spec."
  [u debug]
  (if debug
    (if (spec/valid? :fset/universe u)
      u
      (do (spec/explain :fset/universe u)
          (throw (ex-info "Something went wrong creating the universe. Spec did not match!"
                          {:universe u}))))
    u))


;; (defn varencode
;;   [cfg ir var-id]
;;   (let [{:keys [max-size def-set-size debug]} cfg
;;         ss (b/get-statespace ir)
;;         t (b/get-type ss var-id)
;;         set-id (set-id t)
;;         bools (calc-bools t max-size def-set-size var-id set-id)
;;         var {:id var-id
;;              :type t
;;              :base-set {:id set-id :elems (b/set-elems ss set-id)}
;;              :size (count bools)
;;              :bools bools}]
;;     (-> (->universe ir ss var)
;;         (validate debug)
;;         (encode-invariant)
;;         (encode-initialisation)
;;         (u/rm-var-by-id var-id)
;;         (u/add-bool-vars bools))))
