(defn my-map [f coll]
  (loop [coll coll result '[]]
    (if (empty? coll)
      result
      (recur (rest coll) (conj result (f (first coll)))))))

(my-map inc '(1 2 3))

(defn my-filter [pred coll]
  (loop [coll coll result '[]]
    (if (empty? coll)
      result
      (recur (rest coll) (if (pred (first coll))
                           (conj result (first coll))
                           result)))))

(my-filter even? (range 10))