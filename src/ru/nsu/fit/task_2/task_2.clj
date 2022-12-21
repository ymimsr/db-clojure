(ns ru.nsu.fit.task-2.task-2)

(defn get-smallest-composite
  [elem, elem-factor, factor-map]
  (loop [currElem (+ elem elem-factor)]
    (if (not (some? (get factor-map currElem)))
      currElem
      (recur (+ currElem elem-factor))
    ))
  )

(defn get-next-elem
  [input]
  (let [elem (inc (first input))
        elem-factor (get (nth input 2) elem)
        factor-map (dissoc (nth input 2) elem)]
    (if (some? elem-factor)
      (list elem nil (assoc factor-map (get-smallest-composite elem elem-factor factor-map) elem-factor))
      (list elem elem (assoc factor-map (* elem elem) elem))
      )))


(def primes
    (filter some? (map second (iterate get-next-elem '(1 nil {2 nil}))))
  )

(println (take 100000 primes))

;; primeFactor = Map.Empty
;; for q in (wheel2357):
;;  p = primeFactor.pop(q)
;;  if p is None:
;;    yield q
;;    primeFactor.add(q * q, q)
;;  else
;;    x = p + q
;;    while x in primeFactor:
;;      x += p
;;    primeFactor.add(x, p)