(ns elevator.core-test
  (:require [clojure.test :refer :all]
            [elevator.core :refer :all]))

(deftest downstream-outside-requests
  (are [floor control]
       (= :downstream (discretize? {:floor 4 :direction :down} floor control))
       4 :outside
       3 :outside
       2 :outside
       1 :outside))

(deftest downstream-inside-requests
  (are [floor control]
       (= :downstream (discretize? {:floor 4 :direction :down} floor control))
       3 :inside
       2 :inside
       1 :inside))

(deftest upstream-outside-requests
  (are [floor control]
       (= :upstream (discretize? {:floor 4 :direction :down} floor control))
       5 :outside
       6 :outside
       7 :outside))

