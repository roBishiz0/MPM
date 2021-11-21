(defn cancatOnParts [coll count-parts]
  (let [count-in-part (/ (count coll) count-parts)]
    (loop [count-parts count-parts coll coll result '()]
      (if (or (empty? coll) (< count-parts 0))
        result
        (recur (dec count-parts)
               (drop count-in-part coll)
               (cons (take count-in-part coll) result))))))

(def coll-1 '("a" "aa" "b" "n" "f" "lisp" "clojure" "q" ""))

(take 3 coll-1)

(drop 3 coll-1)

(defn partition-lazy [chunk-size coll]
  (lazy-seq
   (if (empty? coll)
     nil
     (cons
      (take chunk-size coll)
      (partition-lazy
       chunk-size
       (drop chunk-size coll))))))

(defn parallel-filter [pred treads chunk-size coll]
  (mapcat deref
          (doall (map
                  #(future (doall (filter pred %)))
                   (partition-lazy 
                    chunk-size 
                    coll)))))

(defn parallel-filter [pred treads chunk-size coll]
  (mapcat deref
  ;;(mapcat (fn [x] x)
          (mapcat identity
                  (doall (map (fn [colls]
                                (map (fn [pred-colls]
                                       ;;(println pred-colls)
                                       (future (doall (filter pred pred-colls))))
                                     (partition-lazy chunk-size colls)))
                              (partition-lazy
                               (* chunk-size treads)
                               coll))))))

(take 10 (partition-lazy 10 (take 2 (range))))

(parallel-filter (fn [x]
                   (= (count x) 1))
                 4
                 (lazy-seq coll-1))

(time (->> (iterate inc 0)
           (take 20)
           (parallel-filter (fn [x]
                              (Thread/sleep 100)
                              (odd? x))
                            2
                            5)
           (doall)))

(time (->> (iterate inc 0)
           (take 20)
           (filter (fn [x]
                     (Thread/sleep 100)
                     (odd? x)))
           (doall)))

;; "Elapsed time: 0.0886 msecs"


(reductions into '([true false] [true false]))

(remove nil? (map (fn [x state]
                    (when (true? state)
                      x))
                  (lazy-seq coll-1)
                  (lazy-seq coll-2)))

(parallel-filter (fn [x]
                   (= (count x) 1))
                 (lazy-seq coll-1)
                 4)

(defn cancatOnParts [coll count-parts]
  (let [count-in-part (/ (count coll) count-parts)]
    (loop [count-parts count-parts coll coll result '()]
      (if (or (empty? coll) (< count-parts 0))
        result
        (recur (dec count-parts)
               (drop count-in-part coll)
               (cons (take count-in-part coll) result))))))

(def coll-1 '("a" "aa" "b" "n" "f" "lisp" "clojure" "q" ""))

(take 3 coll-1)

(drop 3 coll-1)

(defn partition-lazy [chunk-size coll]
  (lazy-seq
   (if (empty? coll)
     nil
     (cons
      (take chunk-size coll)
      (partition-lazy
       chunk-size
       (drop chunk-size coll))))))

(defn parallel-filter-2 [pred treads chunk-size coll]
  (mapcat deref
          (doall (map
                  #(future (filter pred %))
                  (partition-lazy
                   chunk-size
                   coll)))))

;;;;;;;;;;;;;;;;;;;
(time (->> (iterate inc 0)
           (take 20)
           (parallel-filter-2 (fn [x]
                                (Thread/sleep 100)
                                (odd? x))
                              2
                              5)
           (doall)))

(defn parallel-filter [pred treads chunk-size coll]
  (mapcat deref
  ;;(mapcat (fn [x] x)
          (mapcat (fn [x] x)
                  (doall (map (fn [colls]
                                (map (fn [pred-colls]
                                       (println pred-colls)
                                       (future (filter pred pred-colls)))
                                     (partition-lazy chunk-size colls)))
                              (partition-lazy
                               (* chunk-size treads)
                               coll))))))

(time (->> (iterate inc 0)
           (take 20)
           (parallel-filter-2 (fn [x]
                                (Thread/sleep 100)
                                (odd? x))
                              2
                              5)
           (doall)))

(take 4 (partition-lazy 2 (partition-lazy 3 (range))))

(take 10 (mapcat (fn [x] x) (partition-lazy 1 (range))))

(fn [pred-colls]
  (future (filter pred pred-colls)))

;;;;;;;;;;;;;;;;;;;;;;;;

(time (->> (iterate inc 0)
           (take 20)
           (parallel-filter (fn [x]
                              (Thread/sleep 100)
                              (odd? x))
                            2
                            5)
           (doall)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; test

(time (->> (iterate inc 0)
           (take 20)
           (filter (fn [x]
                     (Thread/sleep 100)
                     (odd? x)))
           (doall)))
