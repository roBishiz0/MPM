(defn func [func index step]
  (reduce (fn [acc x] (* (/ (+ (func acc) (func x)) 2) step))
          (range 0 (+ index 1) step)))

(def parabola (fn [x] (* x x)))

(func parabola 5 1)

(range 0 6 1)

(def func-mem (memoize func))

(defn test-mem [& args]
  (time (func parabola 10 1))
  (time (func-mem parabola 10 1)))