(ns ru.nsu.fit.task-3.task-3)

(def thread-num 4)
(def block-size 100)

(defn heavy-even?
  [num]
  (do
    (Thread/sleep 1)
    (even? num)
   )
  )

(defn filter-by-block
  [predicate, blocks]
  (if (empty? blocks)
    (lazy-seq '())
    (concat
      (let [thread-part-size (int (Math/ceil (/ (count (first blocks)) thread-num)))
            thread-parts (partition-all thread-part-size (first blocks))
            future-filter (fn [part] (future (doall (filter predicate part))))]
        (->> (map future-filter thread-parts)
             (doall)
             (map deref)
             (flatten)
          )
      )
      (lazy-seq (filter-by-block predicate (rest blocks))))
    ))

(defn p-filter
  [predicate, blocks]
  (filter-by-block predicate (partition-all block-size blocks))
  )

;(println (time (take 1000 (p-filter even? (iterate inc 1)))))
;(println (time (take 1000 (filter even? (iterate inc 1)))))

(println (time (doall (take 1000 (p-filter heavy-even? (iterate inc 1))))))
(println (time (doall (take 1000 (filter heavy-even? (iterate inc 1))))))