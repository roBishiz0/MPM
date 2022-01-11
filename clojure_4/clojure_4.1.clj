(ns clojure_4.clojure_4.1)

(defn to-prefix [expr]
  (if (seq? expr)
    (if (= (count expr) 2)
      (cons (first expr) (list (to-prefix (second expr))))
      (let [op (second expr)
            args-raw (map first (partition 2 (concat expr [nil])))
            args (map to-prefix args-raw)]
        (cons op args)))
    (if (and (symbol? expr) (= (first (name expr)) \!))
      (let [name (symbol (subs (name expr) 1))]
        (list 'not name))

      expr)))

(defn to-infix [expr]
  (if (seq? expr)
    (if (= 2 (count expr))
      (if (and (symbol? (second expr)) (= 'not (first expr)))
        (symbol (str "!" (name (second expr))))

        (list (first expr) (to-infix (second expr))))
      (let [op (first expr)
            args (map to-infix (rest expr))]
        (drop-last (interleave args (repeat op)))))
    expr))
