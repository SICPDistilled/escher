(ns escher.t-core
  (:use midje.sweet)
  (:require
;;            [clojure.test :refer :all]
            [escher.core :refer :all])
;;            [escher.core :as core])
  )

(facts "about utility fcns"
  (fact (scale-vec [2 3] 4) =>
         [8 12])
  (fact  (add-vec [-1 2] [3 -2]) =>
          [2 0])
  (fact (sub-vec [2 -3] [-1 -2]) =>
         [3 -1]))
