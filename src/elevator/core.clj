(ns elevator.core)

(def elevator (ref {:floor 10 :direction :down}))

(def microtasks (ref {}))

(def upstream-tasks (ref #{}))

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

(defn merge-task-seq [microtasks & new-tasks]
  (apply merge-with resolve-task-conflicts microtasks new-tasks))

(defn consolidate-tasks [elevator tasks]
  (let [floors (map :floor tasks)
        microtasks (map (partial discretize elevator) floors)]
    (apply merge-task-seq {} microtasks)))

(defn swap-direction [direction]
  ({:up :down :down :up} direction))

(defn consume-upstream-tasks [elevator microtasks upstream-tasks]
  (dosync
   (when (empty? @microtasks)
     (alter elevator assoc :direction (swap-direction (:direction @elevator)))
     (let [new-microtasks (consolidate-tasks @elevator @upstream-tasks)]
       (ref-set microtasks new-microtasks)
       (ref-set upstream-tasks #{})))))

(add-watch microtasks :upstream-consumer
           (fn [_ microtasks-ref _ microtask-coll]
             (consume-upstream-tasks elevator microtasks upstream-tasks)))

(defn submit-request [{:keys [floor location] :as request}]
  (dosync
   (let [result (discretize? @elevator floor location)]
     (cond (= :downstream result) (ref-set microtasks (merge-task-seq @microtasks (discretize @elevator floor)))
           (= :upstream result) (alter upstream-tasks conj request)
           (= :rejected result) (println "Request rejected.")
           :else (println "Unexpected result from discretize?:" result)))))

