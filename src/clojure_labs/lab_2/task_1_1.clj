(ns clojure-labs.lab_2.task_1_1)


(defn trapezoid-area [delegate begin step]
  (* step (/ (+ (delegate begin) (delegate (+ begin step))) 2)) ; (f(a) + f(b)) (b - a)/2 = > (f(a) + f(a + step)) * step / 2
  )




(defn integrate [delegate begin end step]
  (cond
    (>= begin end) 0
    :default (+ (trapezoid-area delegate begin step) (integrate delegate (+ begin step) end step))
    ))



(defn antiderivative [delegate]
  (let [inner-fn (memoize  (partial integrate delegate))]
    (fn [x step] (inner-fn 0 x step))
    )
  )




(defn example-evaluate [x step]
  (let [fun (antiderivative (fn [x] (* (Math/exp x) (Math/sin x))))]
    (time (fun (+ x 1) step))
    )
  )

(defn example-evaluate-two [x step]
  (let [fun (antiderivative (fn [x] (* (Math/exp x) (Math/sin x))))]
    (time (fun x step))
    (time (fun (+ x 1) step))
    )
  )
(println "single x+1")
(example-evaluate 300 1)
(println "x, then x+1 ")
(example-evaluate-two 300 1)
