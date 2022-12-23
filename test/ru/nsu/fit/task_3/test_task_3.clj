(ns ru.nsu.fit.task-3.test-task-3
  (:use ru.nsu.fit.task-3.task-3)
  (:require [clojure.test :as test]))


(test/deftest p-filter-test
  (test/testing "Testing primes"
    ;(let [test-seq (iterate inc 1)]
    ;  (test/is (= (take 1000 ((filter even? test-seq))) (take 1000 (p-filter even? test-seq))))))
    (test/is (=
               (take 1000 (filter even? (iterate inc 1)))
               (take 1000 (p-filter even? (iterate inc 1)))
               )
             )
    ))