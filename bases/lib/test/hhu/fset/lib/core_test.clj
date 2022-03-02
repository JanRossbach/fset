(ns hhu.fset.lib.core-test
  (:require
   [clojure.test :refer [deftest is]]
   [hhu.fset.lib.core :as core]))

(def test-config
  {:max-unroll-size 220
   :unroll-invariant false
   :unroll-sub false
   :deff-set-size 2
   :logging false
   :excluded-vars #{}})

(deftest config-test
  (core/set-config! test-config)
  (is (= test-config (core/get-config)))
  (core/set-config-var! :deff-set-size 3)
  (is (= 3 (:deff-set-size (core/get-config))))
  (core/reset-config)
  (is (not (= test-config (core/get-config)))))
