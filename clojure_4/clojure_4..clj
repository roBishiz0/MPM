(ns clojure_4.clojure_4
  (:use clojure_4.clojure_4.1)
  (:use clojure.set))

(defn find-first
  [f coll]
  (first (filter f coll)))

(defn constant? [expr] (or (= expr true) (= expr false)))

(defn variable? [expr]
  (symbol? expr))

;; Operations

(defn op-named? [name expr]
  (and (seq? expr)
       (>= (count expr) 3)
       (= (first expr) name)))

(def or? "Disjunction" (partial op-named? 'or))

(def and? "Conjunction" (partial op-named? 'and))

(defmacro => [expr1 expr2] `(or (not ~expr1) ~expr2))

(def =>? "implication" (partial op-named? '=>))

(defn not? "Negation" [expr]
  (and (seq? expr)
       (>= (count expr) 2)
       (= (first expr) 'not)))

(defn not-value "Negation" [expr]
  (if (not? expr)
    (second expr)))

(not-value '(no x))

(defn args [expr]
  (rest expr))

(defn flat-if-unary [expr]
  (if (= (count expr) 2)
    (second expr)
    expr))

(defn mutually-exclusive [expr]
  (let [neg
        (->> (args expr)
             (filter not?)
             (map not-value)
             (filter variable?)
             (set))
        pos
        (->> (args expr)
             (filter variable?)
             (set))]

    (not (empty? (intersection neg pos)))))

(declare dnf)
;список правил вывода
(def dnf-rules
  (list
    ; (a => b) => (!a or b)
   [=>?
    (fn [expr]
      (list 'or (list 'not (second expr)) (last expr)))]

; !(a and b and ...) => (!a or !b or ...)
   [(fn [expr] (and (not? expr) (and? (second expr))))
    (fn [expr]
      (cons 'or
            (map (partial list 'not) (args (second expr)))))]

; !(a or b or ...) => (!a and !b and ...)
   [(fn [expr] (and (not? expr) (or? (second expr))))
    (fn [expr]
      (cons 'and
            (map (partial list 'not) (args (second expr)))))]

; (!!a) => (a)
   [(fn [expr] (and (not? expr) (not? (first (args expr)))))
    (fn [expr]
      (first (args (first (args expr)))))]

; true or ... => true
   [(fn [expr] (and (or? expr) (some true? (args expr))))
    (fn [_] true)]

; !x or x or ... => true
   [(fn [expr] (and (or? expr) (mutually-exclusive expr)))
    (fn [_] true)]

    ; !x and x or ... => false
   [(fn [expr] (and (and? expr) (mutually-exclusive expr)))
    (fn [_] false)]

    ; false or ... => ...
   [(fn [expr] (and (or? expr) (some false? (args expr))))
    (fn [expr] (flat-if-unary (cons 'or (remove false? (args expr)))))]

    ; false and ... => false
   [(fn [expr] (and (and? expr) (some false? (args expr))))
    (fn [_] false)]

    ; true and ... => ...
   [(fn [expr] (and (and? expr) (some true? (args expr))))
    (fn [expr] (flat-if-unary (cons 'and (remove true? (args expr)))))]

    ; !true = false, !false = true
   [(fn [expr] (and (not? expr) (constant? (first (args expr)))))
    (fn [expr] (let [value (first (args expr))]
                 (not value)))]

; (a and (b or c)) => ((b and a) or (c and a))
   [(fn [expr]
      (and (and? expr) (some or? (args expr))))
    (fn [expr]
      (let [disj-arg (or (find-first or? (args expr)) false)
            rest-args (remove (partial = disj-arg) (args expr))]
        (cons 'or (map (fn [arg] (cons 'and (cons arg rest-args))) (args disj-arg)))))]

; (a and a and ...) => (a and ...) ,    (a or a or ...) => (a or ...)
   [(fn [expr]
      (and (or (or? expr) (and? expr)) (not (apply distinct? (args expr)))))
    (fn [expr]
       ;(println "RULE: (a and (b and c)) => (a and b and c) ::")
      (let [d-args (distinct (args expr))]
        (if (= (count d-args) 1)
          (first d-args)
          (cons (first expr) d-args))))]

; (a or (b or c)) => (a or b or c)
   [(fn [expr]
      (and (or? expr)  (some or? (args expr))))
    (fn [expr]
       ;(println "RULE: (a or (b or c)) => (a or b or c)  ::")
      (let [inner-expr (find-first or? (args expr))
            c-expr (remove (partial = inner-expr) expr)]
        (concat c-expr (args inner-expr))))]

; (a and (b and c)) => (a and b and c)
   [(fn [expr]
      (and (and? expr)  (some and? (args expr))))
    (fn [expr]
       ;(println "RULE: (a and (b and c)) => (a and b and c) ::" )
      (let [inner-expr (find-first and? (args expr))
            c-expr (remove (partial = inner-expr) expr)]
        (concat c-expr (args inner-expr))))]))

(defn find-applicable-rule [expr]
  (some (fn [rule]
          (if ((first rule) expr)
            (second rule)
            false))
        dnf-rules))

(defn apply-rules-recur [expr]
  (if (or (constant? expr) (variable? expr))
    expr
    (let [expr (cons (first expr) (map apply-rules-recur (args expr)))
          rule (find-applicable-rule expr)]
      (if rule
        (rule expr)
        expr))))

(defn dnf [expr]
  (loop [expr expr prev nil]
    ;(println expr)
    (if (= expr prev)
      expr
      (recur (apply-rules-recur expr) expr))))

(defn pretty-dnf [expr] (to-infix (dnf (to-prefix expr))))

;(pretty-dnf '(x and (x or z) ))
;(println (apply-rules-recur '(or a false )))
;(to-infix '(or (or a (and (not y) y)) x ))
;(dnf (to-prefix '((x or y) and (x or (not y)))))
;(pretty-dnf '(y and ((x or s) and z)))

;(apply-rules-recur '(not (and x y)))
;(to-prefix '(not (x and y)))
;(dnf (to-prefix '(not (x and y))))

;;(let [expr-raw '((x or y) and (x or (not y)))
;;(let [expr-raw '((x or y) and (x or z) )
;(let [expr-raw '(x and (x or z) )
;(let [expr-raw '(y and (x and x))
;      expr (to-prefix expr-raw)
;      ]
;  (println expr)
;  (let [inner-expr (find-first and? (args expr))
;        c-expr (remove (partial = inner-expr) expr)
;        ]
;    (concat c-expr (args inner-expr) )
;    )

  ;(list disj-arg rest-args)
  ;)
;(or? expr)
;(and (or? expr) (mutually-exclusive (args expr)))

;(pretty-dnf '((x => y) and x))
;(apply-rules-recur '(=> x  y))
;(pretty-dnf '(x or y or  (not x)))
;(pretty-dnf '(a or b or (a and b and c and (not c))))
;(pretty-dnf '((x or y) and (x or (not y))))
;(to-prefix '((x or y) and (x or (not y))))
;(dnf (to-prefix '((x or y) and (x or (not y)))))
;(pretty-dnf '((x or y) and (x or z or y)))
;(to-prefix (pretty-dnf '((x or y) and (x or z))))
;(pretty-dnf '(x or (y or z) ))

;(pretty-dnf '(y and (x and x)))
;(pretty-dnf '(y and (x and t)))
;(to-prefix '(y or (!x or x)))
;(to-infix (to-prefix '(!x or x)))