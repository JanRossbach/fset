(ns fset.cli.core-test
  (:require [clojure.test :as test :refer :all]
            [fset.cli.core :as core]
            [clojure.tools.cli :refer [parse-opts]]))

(deftest cli-options-tets
  (is (:help (:options (parse-opts ["-h" "scheduler.mch"] core/cli-options))))
  (is (not (:help (:options (parse-opts ["scheduler.mch"] core/cli-options)))))
  (is (:prob-logging (:options (parse-opts ["-p" "scheduler.mch"] core/cli-options))))
  (is (not (:prob-logging (:options (parse-opts ["scheduler.mch"] core/cli-options)))))
  (is (:logging (:options (parse-opts ["-l" "scheduler.mch"] core/cli-options))))
  (is (not (:logging (:options (parse-opts ["scheduler.mch"] core/cli-options)))))
  (is (= 100 (:max-unroll-size (:options (parse-opts ["-m" "100" "scheduler.mch"] core/cli-options)))))
  (is (seq (:errors (parse-opts ["-m" "-100" "scheduler.mch"] core/cli-options))))
  (is (= 5 (:deff-set-size (:options (parse-opts ["-d" "5" "scheduler.mch"] core/cli-options)))))
  (is (seq (:errors (parse-opts ["-d" "-5" "scheduler.mch"] core/cli-options)))))
