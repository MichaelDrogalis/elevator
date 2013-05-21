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

(defn move-elevator! [elevator-state floor]
  (dosync
   (let [floor-fn (elevator-motion (:direction @elevator-state))]
     (alter elevator-state update-in [:floor] floor-fn)
     (alter microtasks dissoc floor))))

(defmethod perform-task! :proceed
  [floor task]
  (println "---------------------")
  (println "Proceeding to floor" floor)
  (Thread/sleep 2000)
  (move-elevator! elevator floor)
  (println "---------------------"))

(defmethod perform-task! :open-doors
  [floor task]
  (println "---------------------")
  (println "Preparing to stop on floor" floor)
  (Thread/sleep 3000)
  (move-elevator! elevator floor)
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
  (fn [{:keys [floor direction]} downstream-work? dst-floor location]
    [(downstream? direction floor dst-floor) downstream-work? location]))

(defmethod discretize? [true true :outside] [& _] :downstream)

(defmethod discretize? [true true :inside] [& _] :downstream)

(defmethod discretize? [true false :inside] [& _] :downstream)

(defmethod discretize? [false true :outside] [& _] :upstream)

(defmethod discretize? [true false :outside] [& _] :downstream)

(defmethod discretize? [false false :inside] [& _] :upstream)

(defmethod discretize? [false false :outside] [& _] :upstream)

(defmethod discretize? [false true :inside] [& _] :rejected)

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

(add-watch upstream-tasks :upstream-consumer
           (fn [_ _ _ upstream-coll]
             (dosync
              (when (empty? @microtasks)
                (consume-upstream-tasks elevator microtasks @microtasks upstream-tasks)))))

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

(defmulti update-stream! (fn [stream _] stream))

(defmethod update-stream! :downstream
  [_ {:keys [floor]}]
  (let [merged-tasks (merge-task-seq @microtasks (discretize @elevator floor))]
    (ref-set microtasks merged-tasks)))

(defmethod update-stream! :upstream
  [_ request]
  (alter upstream-tasks conj request))

(defmethod update-stream! :rejected
  [_ request]
  (println "Request" request "rejected"))

(defmethod update-stream! :else
  [error request]
  (println "Received error" error "from discretize? for request" request))

(defn submit-request [{:keys [floor location] :as request}]
  (dosync
   (let [downstream-work? (not (empty? @microtasks))
         result (discretize? @elevator downstream-work? floor location)]
     (update-stream! result request))))

