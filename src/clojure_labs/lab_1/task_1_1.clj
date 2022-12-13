(ns clojure-labs.lab_1.task_1_1)

(defn extend-word [word, alphabet, result-word]             ; add to word[n-1] n symbol
  (cond
    (= (count alphabet) 0) result-word
    ; (= (first word) (first alphabet)) (extend-word word (rest alphabet) result-word)
    :default (extend-word
               word
               (rest alphabet)
               (conj result-word (cons (first alphabet) word)))
    )
  )

(defn extend-words [words, alphabet, result-words]          ;add new words
  (cond
    (= (count words) 0) result-words
    :default (extend-words
               (rest words)
               alphabet
               (concat result-words (extend-word (first words) alphabet (list))))

    )
  )

(defn make-words [alphabet max-length]
  (cond
    (= max-length 0) (list (list))
    :default (extend-words (make-words alphabet (dec max-length)) alphabet (list))
    )
  )

(defn evaluate [alphabet max-length]
  (let [result (make-words alphabet max-length)
        alphabet_size (count alphabet)]
    (println (str "expected length: ") (int (* alphabet_size (Math/pow (dec alphabet_size) (dec max-length)))))
    (println (str "evaluated length: " (count result)))
    (println (str "result: ") result))
  )

(evaluate (list "a", "b", "c") 5)
