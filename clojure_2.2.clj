(defn heavy-inc [n]
  (Thread/sleep 100)
  (inc n))

(time
 (->> (iterate inc 0)
      (take 10)
      (map heavy-inc)))

(take 10 (iterate inc 1))

(time
 (->> (iterate inc 0)
      (take 10) ; (0 1 2 3 4 5 6 7 8 9)
      (map heavy-inc) ; (map heavy-inc '(0 1 2 3 4 5 6 7 8 9))
      (doall)))

(defn func [func index step]
  (reduce (fn [acc x] (* (/ (+ (func acc) (func x)) 2) step))
          (range 0 (+ index 1) step)))

(def parabola (fn [x] (* x x)))

(time
 (->> (iterate inc 0)
      (take 10)
      (map #(future (func parabola % 1)))
      (doall)
      (map deref)
      (doall)))