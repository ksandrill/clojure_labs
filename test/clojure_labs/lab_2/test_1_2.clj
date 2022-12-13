(ns clojure-labs.lab_2.test_1_2
  (:require
    [clojure.test :refer [deftest is]]
    [clojure_labs.lab_2.task_1_2 :refer [antiderivative]]))


(deftest zero-x
  (is (= 0 ((antiderivative (fn [x] (* x 2)) 1) 0)))
  )

(deftest zero-fun
  (is (= 0 ((antiderivative (fn [_] 0) 1) 10)))
  )

(deftest negative-x
  (is (= 0 ((antiderivative (fn [x] (* x 2)) -1) -1)))
  )

(deftest negative-fun
  (is (= -100 ((antiderivative (fn [x] (* x -2)) 1) 10)))
  )

(deftest test-1
  (is (= 100 ((antiderivative (fn [x] (* x 2)) 1) 10)))
  )

(deftest test-2
  (is (= 335 ((antiderivative (fn [x] (* x x) ) 1) 10)))
  )
