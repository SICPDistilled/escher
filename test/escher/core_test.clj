(ns escher.core-test
  (:require [clojure.test :refer :all]
            [escher.core :refer :all]))

(deftest vectors
  (is (= (scale-vec [2 3] 4)
         [8 12]))
  (is (=  (add-vec [-1 2] [3 -2])
          [2 0]))
  (is (= (sub-vec [2 -3] [-1 -2])
         [3 -1])))
