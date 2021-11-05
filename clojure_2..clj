(defn func [func index step]
  (reduce (fn [acc x2] (+ acc (* (func x2) step)))
          (conj (vec (range 0 index step)) index)))

(def parabola (fn [x] (* x x)))

(func parabola 5 1)

(range 0 6 1)