(defn gen-word-plus-alph [word alph]
  (reduce (fn [acc letter] (if (= (first word) letter)
                             acc
                             (cons (cons letter word) acc)))
          '()
          alph))

(defn gen-word-plus-words [alph words]
  (reduce into (map #(gen-word-plus-alph % alph) words)))

(defn gen [alph n]
  (reduce (fn [acc _] (gen-word-plus-words alph acc)) [[]] (range n)))

(gen '(:a :b :c) 3)