(ns ru.nsu.fit.task-2.test-task-2
  (:use ru.nsu.fit.task-2.task-2)
  (:require [clojure.test :as test]))

(test/deftest primes_test
  (test/testing "Testing primes"
    (let [test_primes (set (take 100000 primes))]
      (test/is (contains? test_primes 2))
      (test/is (contains? test_primes 3))
      (test/is (contains? test_primes 5))
      (test/is (contains? test_primes 191))
      (test/is (contains? test_primes 125141))
      (test/is (contains? test_primes 398113))))
  )