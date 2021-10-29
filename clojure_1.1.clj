(defn gen-word-plus-alph [word alph]
  (if (empty? alph)
    ()
    (if (= (first word) (first alph))
      (gen-word-plus-alph word (rest alph))
      (cons (cons (first alph) word) (gen-word-plus-alph word (rest alph))))))

(defn gen-word-plus-words [word words]
  ;(loop [words words result '(())]
  (if (empty? words)
    ()
    (concat
     (gen-word-plus-alph (first words) word)
     (gen-word-plus-words word (rest words)))))

(defn gen [alph n]
  (loop [n n words '(())]
    (if (= n 0)
      words
      (recur (dec n) (gen-word-plus-words alph words)))))

(gen '(:a :b :c) 3)