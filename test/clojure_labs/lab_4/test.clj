(ns clojure_labs.lab_4.test
  (:require [clojure.test :refer [deftest is]]
            [clojure_labs.lab_4.core :refer [c-and
                                             c-impl
                                             c-var
                                             c-const
                                             c-not
                                             c-or
                                             c-equal
                                             to-dnf
                                             view
                                             compare-exprs
                                             ]]))


(deftest dnf-easy1-test
  (let [test-expr (c-and (c-impl (c-var "a") (c-const false)) (c-var "d"))
        res (to-dnf test-expr)
        should "(!a & d)"]
    (is (= should (view res)))
    (is (true? (compare-exprs test-expr res)))
    )
  )

(deftest dnf-easy2-test
  (let [test-expr (c-or (c-var "x") (c-and (c-var "x") (c-var "y")))
        res (to-dnf test-expr)
        should "x"]
    (is (= should (view res)))
    (is (true? (compare-exprs test-expr res)))
    )
  )

(deftest dnf-easy3-test
  (let [test-expr (c-and (c-var "x") (c-or (c-var "x") (c-var "y")))
        res (to-dnf test-expr)
        should "x"]
    (is (= should (view res)))
    (is (true? (compare-exprs test-expr res)))
    )
  )

(deftest dnf-easy4-test
  (let [test-expr (c-and (c-var "x") (c-or (c-not (c-var "x")) (c-var "y")))
        res (to-dnf test-expr)
        should "(x & y)"]
    (is (= should (view res)))
    (is (true? (compare-exprs test-expr res)))
    )
  )

(deftest dnf-easy5-test
  (let [test-expr (c-or (c-var "x") (c-and (c-not (c-var "x")) (c-var "y")))
        res (to-dnf test-expr)
        should "(x | y)"]
    (is (= should (view res)))
    (is (true? (compare-exprs test-expr res)))
    )
  )

(deftest dnf-easy6-test
  (let [test-expr (c-and (c-not (c-var "x")) (c-or (c-var "x") (c-var "y")))
        res (to-dnf test-expr)
        should "(!x & y)"]
    (is (= should (view res)))
    (is (true? (compare-exprs test-expr res)))
    )
  )

(deftest dnf-easy7-test
  (let [test-expr (c-or (c-not (c-var "x")) (c-and (c-var "x") (c-var "y")))
        res (to-dnf test-expr)
        should "(!x | y)"]
    (is (= should (view res)))
    (is (true? (compare-exprs test-expr res)))
    )
  )

(deftest dnf-complex-test
  (let [test-expr (c-and (c-impl (c-not (c-or (c-var "a") (c-not (c-var "k")))) (c-var "b")) (c-equal (c-var "c") (c-var "d")))
        res (to-dnf test-expr)
        should "((a & !d & !c) | (b & d & c) | (b & !d & !c) | (!k & d & c) | (!k & !d & !c) | (a & d & c))"]
    (is (= should (view res)))
    (is (true? (compare-exprs test-expr res)))
    )
  )