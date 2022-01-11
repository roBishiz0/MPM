(defn partition-lazy [chunk-size coll]
  (lazy-seq
   (if (empty? coll)
     nil
     (cons
      (take chunk-size coll)
      (partition-lazy
       chunk-size
       (drop chunk-size coll))))))

(cancatOnParts (take 10000 (range)) 4)

(defn parallel-filter [pred treads chunk-size coll]
  (mapcat deref
  ;;(mapcat (fn [x] x)
          (mapcat identity
                  (map (fn [colls]
                         (doall
                          (map (fn [pred-colls]
                                 ;;(println pred-colls)
                                 (future (doall (filter pred pred-colls))))
                               (partition-lazy chunk-size colls))))
                         (partition-lazy
                          (* chunk-size treads)
                          coll)))))

(take 3 (range))

(take 4 coll)

(take 4 (drop 4 coll))

(print "test")
