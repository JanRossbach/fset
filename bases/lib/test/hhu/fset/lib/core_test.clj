(ns hhu.fset.lib.core-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [hhu.fset.backend.interface :refer [model-check]]
   [hhu.fset.encoder.test-machines :refer [empty-ir scheduler numbers train]]
   [lisb.translation.util :refer [ir->b b->ir]]
   [hhu.fset.lib.core :as core]))

(deftest dummy-test
  (is (= 1 1)))
