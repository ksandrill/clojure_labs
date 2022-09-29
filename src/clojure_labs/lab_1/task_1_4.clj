(ns clojure-labs.lab-1.task-1-4)



(defn extend-word [word, alphabet]
  (map
    (fn [x] (cons x word))
    (filter (fn [x] (not= (first word) x)) alphabet))
  )


(defn extend-words [words alphabet]
  (reduce
    concat
    (map (fn [x] (extend-word x alphabet)) words))
  )


(defn make-words [alphabet max-length]
  (nth
    (iterate (fn [acc] (extend-words acc alphabet)) (list (list)))
    max-length)
  )

(defn evaluate [alphabet max-length]
  (let [result (make-words alphabet max-length)
        alphabet_size (count alphabet)]
    (println (str "expected permutation's number: ")
             (int (* alphabet_size (Math/pow (dec alphabet_size) (dec max-length)))))
    (println (str "evaluated permutation's number: " (count result)))
    (println (str "result: ") result))
  )

(evaluate (list "a", "b") 5)