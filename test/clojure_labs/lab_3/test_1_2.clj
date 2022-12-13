

(ns clojure-labs.lab_3.test_1_2
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-labs.lab_3.task_1_2 :refer [lazy-partition*
                                    lazy-parallel-filter]]))


(deftest lazy-partition*-test
  (testing "Empty collection"
    (is (=
          (lazy-partition* 10 [])
          [])))

  (testing "Bad chunk size"
    (is (nil? (lazy-partition* -1 [1 2 3])))
    (is (nil? (lazy-partition* 0 [1 2 3]))))

  (testing "Coll length is multiple of chunk size"
    (is (=
          (lazy-partition* 1 [1 2 3])
          [[1] [2] [3]]))
    (is (=
          (lazy-partition* 2 [1 2 3 4])
          [[1 2] [3 4]])))

  (testing "Coll length is not multiple of chunk size"
    (is (=
          (lazy-partition* 2 [1 2 3])
          [[1 2] [3]]))
    (is (=
          (lazy-partition* 5 [1 2 3 4])
          [[1 2 3 4]]))
    (is (=
          (lazy-partition* 3 [1 2 3 4 5 6 7 8])
          [[1 2 3] [4 5 6] [7 8]])))

  (testing "Infinite sequence"
    (is (=
          (take 0 (lazy-partition* 3 (range)))
          []))

    (is (=
          (take 3 (lazy-partition* 3 (range)))
          [[0 1 2] [3 4 5] [6 7 8]]))))

(deftest lazy-parallel-filter-test
  (testing "Empty coll"
    (is (= (lazy-parallel-filter even? [])
           [])))

  (testing "Non-empty finite coll"
    (doseq [[pred coll] [[even? (range 1000)]
                         [odd? (range 10000)]
                         [(fn [it] (.contains it "42")) (repeat 10000 "11111")]]]
      (let [expected (filter pred coll)]
        (is (= (lazy-parallel-filter pred coll 300)
               expected))

        (is (= (lazy-parallel-filter pred coll 100)
               expected)))))

  (testing "Non-empty infinite coll"
    (doseq [[pred coll n] [[even? (range) 1000]
                           [odd? (range) 9999]
                           [(fn [it] (.contains it "42")) (repeat "11111 42") 5000]]]
      (let [expected (take n (filter pred coll))]
        (is (= (take n (lazy-parallel-filter pred coll 300))
               expected))

        (is (= (take n (lazy-parallel-filter pred coll 100))
               expected))))))


