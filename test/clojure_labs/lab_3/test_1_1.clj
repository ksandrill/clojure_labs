
(ns clojure_labs.lab_3.test_1_1
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-labs.lab_3.task_1_1 :refer [partition* parallel-filter]]))


(deftest partition*-test
  (testing "Empty collection"
    (is (=
          (partition* 10 [])
          [])))

  (testing "Bad chunk size"
    (is (nil? (partition* -1 [1 2 3])))
    (is (nil? (partition* 0 [1 2 3]))))

  (testing "Coll length is multiple of chunk size"
    (is (=
          (partition* 1 [1 2 3])
          [[1] [2] [3]]))
    (is (=
          (partition* 2 [1 2 3 4])
          [[1 2] [3 4]])))

  (testing "Coll length is not multiple of chunk size"
    (is (=
          (partition* 2 [1 2 3])
          [[1 2] [3]]))
    (is (=
          (partition* 5 [1 2 3 4])
          [[1 2 3 4]]))
    (is (=
          (partition* 3 [1 2 3 4 5 6 7 8])
          [[1 2 3] [4 5 6] [7 8]]))))

(deftest parallel-filter-test
  (testing "Empty coll"
    (is (= (parallel-filter even? [])
           [])))

  (testing "Non-empty coll"
    (doseq [[pred coll] [[even? (range 1000)]
                         [odd? (range 10000)]
                         [(fn [it] (.contains it "42")) (repeat 10000 "11111")]]]
      (let [expected (filter pred coll)]
        (is (= (parallel-filter pred coll 300)
               expected))

        (is (= (parallel-filter pred coll 100)
               expected))))))


