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

(deftest safely-merges-task-seqs
  (is (= {4 :proceed 3 :proceed 2 :open-doors 1 :open-doors}
         (merge-task-seq {4 :proceed 3 :proceed 2 :proceed 1 :open-doors}
                         {4 :proceed 3 :proceed 2 :open-doors}))))

(deftest discretizes-and-merges-upstream-tasks
  (is (= {4 :proceed 3 :open-doors 2 :open-doors}
         (consolidate-tasks {:floor 5 :direction :down}
                            #{{:floor 3} {:floor 2}}))))

(deftest swap-vertical-direction
  (are [old new] (= new (swap-direction old))
       :down :up
       :up :down))

(deftest upstream-tasks-not-consumed
  (let [elevator (ref {:floor 5 :direction :down})
        microtasks (ref {1 :open-doors})
        upstream-tasks (ref #{{:floor 6}})]
    (consume-upstream-tasks elevator microtasks upstream-tasks)
    (is (= #{{:floor 6}} @upstream-tasks))
    (is (= :down (:direction @elevator)))))

(deftest upstream-tasks-consumed
  (let [elevator (ref {:floor 5 :direction :down})
        microtasks (ref {})
        upstream-tasks (ref #{{:floor 6}})]
    (consume-upstream-tasks elevator microtasks upstream-tasks)
    (is (= #{} @upstream-tasks))
    (is (= {6 :open-doors} @microtasks))
    (is (= :up (:direction @elevator)))))

