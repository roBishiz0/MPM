(defn cancatOnParts [coll count-parts]
  (let [count-in-part (/ (count coll) count-parts)]
  (loop [count-parts count-parts coll coll result '()]
    (if (or (empty? coll) (< count-parts 0))
      result
      (recur (dec count-parts)
             (drop count-in-part coll)
             (cons (take count-in-part coll) result))))))

(def coll '("a" "aa" "b" "n" "f" "lisp" "clojure" "q" ""))

(cancatOnParts '("a" "aa" "b" "n" "f" "lisp" "clojure" "q" "" "d" "e" "s" "q") 4)

(defn parallel-filter [pred coll]
  (map #(future (map pred %)) (cancatOnParts coll 4)))

(defn parallel-filter [pred function limit]
  (map #(future (map pred %)) (cancatOnParts (take limit function) 4)))

(cancatOnParts (take 10000 (range)) 4)

(parallel-filter (fn [x] (= (count x) 1)) (range) 10000)

(take 3 (range))

(take 4 coll)

(take 4 (drop 4 coll))

(/ (count coll) 4)

(let [coll ["a" "aa" "b" "n" "f" "lisp" "clojure" "q" ""]])

(filter
 #(= (count %) 1)
 ["a" "aa" "b" "n" "f" "lisp" "clojure" "q" ""])

(parallel-filter
 #(= (count %) 1)
 ["a" "aa" "b" "n" "f" "lisp" "clojure" "q" ""])