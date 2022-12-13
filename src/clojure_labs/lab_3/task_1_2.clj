(ns clojure-labs.lab_3.task_1_2)
(defn lazy-partition*
  [chunk-size coll]
  (if (<= chunk-size 0)
    nil
    (lazy-seq
      (if (empty? coll)
        nil
        (cons
          (take chunk-size coll)
          (lazy-partition* chunk-size (drop chunk-size coll)))))))


(defn lazy-parallel-filter
  ([pred coll]
   (lazy-parallel-filter pred coll 1000))
  ([pred coll chunk-size]
   (let [runtime (Runtime/getRuntime)
         threads-count (.availableProcessors runtime)]
     (->>
       (lazy-partition* chunk-size coll)
       (map #(future (doall (filter pred %))))
       (lazy-partition* threads-count)
       (map doall)
       (mapcat identity)
       (mapcat deref)))))

;
(defn -main
  []
  (let [very-long-string (str (reduce str (repeat 10 "123456789")) " 42")
        make-coll #(repeat very-long-string)
        pred (fn [s]
               (Thread/sleep 1)
               (.contains s "42"))
        n 10000]
    (println (first (time (doall (take n (filter pred (make-coll)))))))
    (println (first (time (doall (take n (lazy-parallel-filter pred (make-coll)))))))
    ; "Elapsed time: 3203.021099 msecs"
    ; "Elapsed time: 1359.014099 msecs"
    (System/exit 0)))


;(defn -main
;  []
;  (let [coll (list (range 0 100000 1))
;        pred (fn [s]
;               (Thread/sleep 1)
;               even? s)
;        n 1]
;    (println (first (time (doall (take n (filter pred coll))))))
;    (println (first (time (doall (take n (lazy-parallel-filter pred coll))))))
;    ; "Elapsed time: 3203.021099 msecs"
;    ; "Elapsed time: 1359.014099 msecs"
;    (System/exit 0)))
