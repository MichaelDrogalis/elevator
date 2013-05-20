(ns elevator.core-test
  (:require [clojure.test :refer :all]
            [elevator.core :refer :all]))

(def discretize?> (partial discretize? {:floor 4 :direction :down}))

(deftest downstream-outside-requests
  (are [floor]
    (= :downstream (discretize?> floor :outside))
    4 3 2 1))

(deftest downstream-inside-requests
  (are [floor]
    (= :downstream (discretize?> floor :inside))
    4 3 2 1))

(deftest upstream-outside-requests
  (are [floor]
    (= :upstream (discretize?> floor :outside))
    5 6 7))

(deftest rejected-inside-requests
  (are [floor]
    (= :rejected (discretize?> floor :inside))
    5 6 7))

(deftest discretizes-downward-microtasks
  (are [dst-floor tasks]
    (= tasks (discretize {:floor 5 :direction :down} dst-floor))
    4 {4 :open-doors}
    3 {4 :proceed 3 :open-doors}
    2 {4 :proceed 3 :proceed 2 :open-doors}))

(deftest discretizes-upward-microtasks
  (are [dst-floor tasks]
    (= tasks (discretize {:floor 5 :direction :up} dst-floor))
    6 {6 :open-doors}
    7 {6 :proceed 7 :open-doors}
    8 {6 :proceed 7 :proceed 8 :open-doors}))

(deftest conflicts-resolved-safely
  (are [scheduled candidate expectations]
    (= expectations (resolve-task-conflicts scheduled candidate))
    :proceed    :open-doors :open-doors  
    :open-doors :proceed    :open-doors  
    :open-doors :open-doors :open-doors  
    :proceed    :proceed    :proceed))

