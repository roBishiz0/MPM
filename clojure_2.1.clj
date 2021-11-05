(defn func [func index step]
  (loop [x1 0 x2 step result 0]
    (if (< x2 index)
      (recur
       x2
       (+ x2 step)
       (+ result (* (/ (+ (func x1) (func x2)) 2) step)))
      result)))

(defn integrate [func step]
  (reductions (fn [acc x2] (+ acc (*
                                   (/ (+ (func x2) (func (- x2 step)))
                                      2)
                                   step)))
              (map (partial * step) (range))))

(defn integrator [upper-limit]
 (let [step 1
       function identity
       integrator (integrate function step)]
   (+ (* (function (/ (+ (function (int upper-limit)) (function upper-limit)) 2))
         (- upper-limit (int upper-limit)))
      (nth integrator upper-limit))))

(def integrator-mem (memoize integrator))

(do
  (time (integrator 1000000))
  (time (integrator 1000000))
  (time (integrator 1000000))
  (time (integrator 1000000))
  (time (integrator 1000000))
  (time (integrator 1000000)))

(do
  (time (integrator-mem 1000000))
  (time (integrator-mem 1000000))
  (time (integrator-mem 1000000))
  (time (integrator-mem 1000000))
  (time (integrator-mem 1000000))
  (time (integrator-mem 1000000)))

(def func-mem (memoize func)) ; Ошибка