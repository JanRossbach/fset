(ns fset.core-test
  (:require
   [clojure.test :refer :all]
   [lisb.translation.util :refer [lisb->ir]]
   [fset.core :refer :all]))

(def empty-machine-path "resources/machines/test/b/empty.mch")
(def empty-machine '(machine
                     (machine-variant)
                     (machine-header :Empty [])))

(deftest load-empty-machine-test
  (testing "load-mch! sucessfully loads the empty machine."
    (let [m (load-mch! empty-machine-path)]
      (is (and (contains? m :ir)
               (contains? m :ss)
               (contains? m :meta))))))

(deftest load-mch-preserves-meta-data-test
  (testing "load-mch! preserves given meta-data"
    (let [m (load-mch! "resources/machines/test/b/empty.mch" {:some "meta-data"})]
      (is (= (:meta m) {:some "meta-data"})))))


(deftest make-mch-preserves-meta-data-test
  (testing "load-mch! preserves given meta-data"
    (let [m (make-mch! (lisb->ir empty-machine) {:some "meta-data"})]
      (is (= (:meta m) {:some "meta-data"})))))
