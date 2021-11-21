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

(time
 (->> (iterate inc 0)
      (take 1000)
      (map #(future (integrator %)))
      (doall)
      (map deref)
      (doall)))

(time
 (->> (iterate inc 0)
      (take 1000)
      (map #(future (integrator %)))
      (map deref)
      (doall)))