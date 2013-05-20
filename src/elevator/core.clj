(ns elevator.core)

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

(defmethod discretize? [true :outside]
  [& _] :downstream)

(defmethod discretize? [false :outside]
  [& _] :upstream)

(defmethod discretize? [true :inside]
  [& _] :downstream)

(defmethod discretize? [false :inside]
  [& _] :rejected)

(defmulti discretize
  (fn [{:keys [direction]} _]
    direction))

(defn proceeding-tasks [floor-seq]
  (reduce
   (fn [tasks next-floor]
     (conj tasks {:floor next-floor :task :proceed}))
   [] floor-seq))

(defmethod discretize :up
  [{:keys [floor]} dst-floor]
  (conj (proceeding-tasks (range (inc floor) dst-floor))
        {:floor dst-floor :task :open-doors}))

(defmethod discretize :down
  [{:keys [floor]} dst-floor]
  (conj (proceeding-tasks (reverse (range (inc dst-floor) floor)))
        {:floor dst-floor :task :open-doors}))

