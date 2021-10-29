(defn gen-word-plus-alph [word alph]
  (loop [alph alph result '()]
    (if (empty? alph)
      result
      (if (= (first word) (first alph))
        (recur (rest alph) result)
        (recur (rest alph) (cons (cons (first alph) word) result))))))

(defn gen-word-plus-words [word words]
  (loop [words  words
         result '()]
    (if (empty? words)
      result
      (recur (rest words) (concat result (gen-word-plus-alph (first words) word))))))

(defn gen [alph n]
  (loop [n n words '(())]
    (if (= n 0)
      words
      (recur (dec n) (gen-word-plus-words alph words)))))

(gen '(:a :b :c) 3)