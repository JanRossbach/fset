(ns fset.backend.core
  (:require [clojure.core.matrix :as m]))

(m/set-current-implementation :vectorz) ;; switch to vectorz vectors in order to improve performance

(m/add [[1 2]
     [3 4]]
   (m/mmul (m/identity-matrix 2) 3.0))

(m/shape [[2 3 4] [5 6 7]])

(m/mmul
 (m/array [[2 2] [3 3]])
 (m/array [[4 4] [5 5]])) ; => [[18 18] [27 27]]
