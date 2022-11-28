(ns ru.nsu.fit.task-1.task-1)

;; returns an alphabet without letters that are present in given word
(defn get-sub-alphabet
  [word, alphabet]
  (remove
    (fn [x] (.contains word x))
    alphabet)
  )

;; returns a list with new words from given word
(defn get-new-words
  [word, alphabet]
  (map
    (fn [x] (str word x))
    (get-sub-alphabet word alphabet))
  )


(defn get-words
  [n, alphabet]
  (reduce
        (fn [acc, _] (reduce concat (map (fn [word] (get-new-words word alphabet)) acc)))
        alphabet (range 1 n)
        )
  )


(println (get-words 4 (list "a" "b" "c" "d")))