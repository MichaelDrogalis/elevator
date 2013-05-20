(ns elevator.core)

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

