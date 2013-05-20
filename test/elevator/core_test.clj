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

(deftest discretizes-microtasks
  (is (= [{:floor 4 :task :open-doors}]
         (discretize {:floor 5 :direction :down} 4)))
  (is (= [{:floor 4 :task :proceed} {:floor 3 :task :open-doors}]
         (discretize {:floor 5 :direction :down} 3)))
  (is (= [{:floor 4 :task :proceed}
          {:floor 3 :task :proceed}
          {:floor 2 :task :open-doors}]
         (discretize {:floor 5 :direction :down} 2))))

