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