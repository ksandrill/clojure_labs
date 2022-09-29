(ns clojure-labs.lab-1.task-1-2)

(defn extend-word [word, alphabet, result-word]             ; add to word[n-1] n symbol
  (cond
    (= (count alphabet) 0) result-word
    (= (first word) (first alphabet)) (recur word (rest alphabet) result-word)
    :default (recur
               word
               (rest alphabet)
               (conj result-word (cons (first alphabet) word)))
    )
  )

(defn extend-words [words, alphabet, result-words]          ;add new words
  (cond
    (= (count words) 0) result-words
    :default (recur
               (rest words)
               alphabet
               (concat result-words (extend-word (first words) alphabet (list))))

    )
  )

(defn make-words [alphabet max-length res]
  (cond
    (= max-length 0) res
    :default (recur alphabet (dec max-length) (extend-words res alphabet (list)))
    )
  )


(defn evaluate [alphabet max-length]
  (let [result (make-words alphabet max-length (list (list)))
        alphabet_size (count alphabet)]
    (println (str "expected permutation's number: ")
             (int (* alphabet_size (Math/pow (dec alphabet_size) (dec max-length)))))
    (println (str "evaluated permutation's number: " (count result)))
    (println (str "result: ") result))
  )

(evaluate (list "a", "b") 3)