(ns  clojure-labs.lab_3.task_1_1)

(defn partition*
  [chunk-size coll]
  (if (<= chunk-size 0)
    nil
    (loop [coll coll
           result []]
      (if (empty? coll)
        result
        (recur
          (drop chunk-size coll)
          (conj result (take chunk-size coll)))))))


(defn parallel-filter
  ([pred coll]
   (parallel-filter pred coll 100))
  ([pred coll chunk-size]
   (->>
     (partition* chunk-size coll)
     (map #(future (doall (filter pred %))))
     (doall)
     (mapcat deref))))

(defn -main
  []
  (let [very-long-string (str (reduce str (repeat 10 "123456789")) " 42")
        coll (doall (repeat 10000 very-long-string))
        pred (fn [s]
               (Thread/sleep 1)
               (.contains s "42"))]
    (println (first (time (doall (filter pred coll)))))
    (println (first (time (doall (parallel-filter pred coll)))))
    ; "Elapsed time: 1837.0365 msecs"
    ; "Elapsed time: 869.625399 msecs"
    (System/exit 0)))



;(defn -main
;  []
;  (let [
;        coll (list (range 0 100 1))
;        pred (fn [s]
;               ;(Thread/sleep 1)
;               even? s)]
;    (println (first (time (doall (filter pred coll)))))
;    (println (first (time (doall (parallel-filter pred coll)))))
;
;    (System/exit 0)))