(ns elevator.core-test
  (:require [clojure.test :refer :all]
            [elevator.core :refer :all]))

(def discretize?> (partial discretize? {:floor 4 :direction :down}))

(deftest downstream-outside-requests
  (are [floor control]
       (= :downstream (discretize?> floor control))
       4 :outside
       3 :outside
       2 :outside
       1 :outside))

(deftest downstream-inside-requests
  (are [floor control]
       (= :downstream (discretize?> floor control))
       4 :inside
       3 :inside
       2 :inside
       1 :inside))

(deftest upstream-outside-requests
  (are [floor control]
       (= :upstream (discretize?> floor control))
       5 :outside
       6 :outside
       7 :outside))

(deftest rejected-inside-requests
  (are [floor control]
       (= :rejected (discretize?> floor control))
       5 :inside
       6 :inside
       7 :inside))

