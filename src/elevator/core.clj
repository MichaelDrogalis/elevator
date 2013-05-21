(ns elevator.core
  (:require [clojure.data :refer [diff]]
            [lamina.core :refer [channel enqueue receive-all]]))

(def elevator (ref {:floor 10 :direction :down}))

(def microtasks (ref {}))

(def upstream-tasks (ref #{}))

(def microtask-queue (channel))

(defmulti perform-task!
  (fn [_ task] task))

(def elevator-motion {:up inc :down dec})

(defn move-elevator! [elevator-state]
  (dosync (alter elevator-state update-in [:floor]
                 (elevator-motion (:direction @elevator-state)))))

(defmethod perform-task! :proceed
  [floor task]
  (println "---------------------")
  (println "Proceeding to floor" floor)
  (Thread/sleep 2000)
  (move-elevator! elevator)
  (println "---------------------"))

(defmethod perform-task! :open-doors
  [floor task]
  (println "---------------------")
  (println "Preparing to stop on floor" floor)
  (Thread/sleep 3000)
  (move-elevator! elevator)
  (println "---------------------"))

(defn elevator-consumer [f]
  (let [{:keys [floor task]} (f)]
    (perform-task! floor task)))

(receive-all microtask-queue elevator-consumer)

(defmulti downstream?
  (fn [direction _ _] direction))

(defmethod downstream? :up
  [_ src-floor dst-floor]
  (>= (- dst-floor src-floor) 0))

(defmethod downstream? :down
  [_ src-floor dst-floor]
  (>= (- src-floor dst-floor) 0))

(defmulti discretize?
  (fn [{:keys [floor direction]} dst-floor location]
    [(downstream? direction floor dst-floor) location]))

(defmethod discretize? [true :outside] [& _] :downstream)

(defmethod discretize? [false :outside] [& _] :upstream)

(defmethod discretize? [true :inside] [& _] :downstream)

(defmethod discretize? [false :inside] [& _] :rejected)

(defmulti discretize
  (fn [{:keys [direction]} _]
    direction))

(defn proceeding-tasks [floor-seq]
  (reduce
   (fn [tasks next-floor]
     (assoc tasks next-floor :proceed))
   {} floor-seq))

(defn open-doors-task [floor]
  {floor :open-doors})

(defn task-seq [floor-seq dst-floor]
  (merge (proceeding-tasks floor-seq)
         (open-doors-task dst-floor)))

(defmethod discretize :up
  [{:keys [floor]} dst-floor]
  (task-seq (range (inc floor) dst-floor) dst-floor))

(defmethod discretize :down
  [{:keys [floor]} dst-floor]
  (task-seq (reverse (range (inc dst-floor) floor)) dst-floor))

(defn resolve-task-conflicts [scheduled-task candidate-task]
  (if (some (partial = :open-doors) [scheduled-task candidate-task])
    :open-doors
    :proceed))

(defn merge-task-seq [microtasks-coll & new-tasks]
  (apply merge-with resolve-task-conflicts microtasks-coll new-tasks))

(defn consolidate-tasks [elevator tasks]
  (let [floors (map :floor tasks)
        microtask-seq (map (partial discretize elevator) floors)]
    (apply merge-task-seq {} microtask-seq)))

(defn swap-direction [direction]
  ({:up :down :down :up} direction))

(defn consume-upstream-tasks [elevator microtasks-ref microtasks-coll upstream-tasks]
  (dosync
   (when (and (empty? microtasks-coll) (not (empty? @upstream-tasks)))
     (alter elevator assoc :direction (swap-direction (:direction @elevator)))
     (let [new-microtasks (consolidate-tasks @elevator @upstream-tasks)]
       (ref-set microtasks-ref new-microtasks)
       (ref-set upstream-tasks #{})))))

(add-watch microtasks :upstream-consumer
           (fn [_ microtasks-ref _ microtask-coll]
             (consume-upstream-tasks elevator microtasks-ref microtask-coll upstream-tasks)))

(defn new-microtasks [old-tasks new-tasks]
  (or (second (diff old-tasks new-tasks)) {}))

(def directional-comparators {:up identity :down reverse})

(defn sort-microtasks [direction tasks]
  ((get directional-comparators direction) (sort (keys tasks))))

(defn enqueueable-microtasks [old-tasks new-tasks direction]
  (sort-microtasks direction (new-microtasks old-tasks new-tasks)))

(add-watch microtasks :enqueuer
           (fn [_ _ old-tasks new-tasks]
             (let [tasks (enqueueable-microtasks old-tasks new-tasks (:direction @elevator))]
               (doseq [floor tasks]
                 (enqueue microtask-queue
                          (fn [] {:floor floor :task (get @microtasks floor)}))))))

(defn submit-request [{:keys [floor location] :as request}]
  (dosync
   (let [result (discretize? @elevator floor location)]
     (cond (= :downstream result) (ref-set microtasks (merge-task-seq @microtasks (discretize @elevator floor)))
           (= :upstream result) (alter upstream-tasks conj request)
           (= :rejected result) (println "Request rejected.")
           :else (println "Unexpected result from discretize?:" result)))))

