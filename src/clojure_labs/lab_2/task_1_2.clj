(ns clojure_labs.lab_2.task_1_2)
(defn trapezoid-area [delegate begin step]
  (* step (/ (+ (delegate begin) (delegate (+ begin step))) 2)) ; (f(a) + f(b)) (b - a)/2 = > (f(a) + f(a + step)) * step / 2
  )

(defn antiderivative [func step]
  (let [
        inner-fun (fn integral-fun ([step] (integral-fun 0 step))
                    ([begin step] (cons (trapezoid-area func begin step) (lazy-seq (integral-fun (+ begin step) step)))))
        aux-func (reductions + (inner-fun step))]
    (fn [x]
      (if (> x 0)
        (nth aux-func (- (/ x step) 1))
        0))))

(defn example-evaluate-one [x step]
  (let [fun (antiderivative (fn [x] (* (Math/exp x) (Math/sin x))) step)]
    (time (fun (+ x 1)))
    )
  )

(defn example-evaluate-two [x step]
  (let [fun (antiderivative (fn [x] (* (Math/exp x) (Math/sin x))) step)]
    (time (fun x))
    (time (fun (+ x 1)))
    )
  )

(println "single x+1")
(example-evaluate-one 300 1)
(println "x, then x+1 ")
(example-evaluate-two 300 1)

