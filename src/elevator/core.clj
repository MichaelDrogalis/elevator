(ns elevator.core)

(defmulti discretize?
  (fn [{:keys [floor direction]} dst-floor location]
    (if (= :up direction)
      [(>= (- dst-floor floor) 0) location]
      [(>= (- floor dst-floor) 0) location])))

(defmethod discretize? [true :outside]
  [& _]
  :downstream)

(defmethod discretize? [false :outside]
  [& _]
  :upstream)

(defmethod discretize? [true :inside]
  [& _]
  :downstream)

