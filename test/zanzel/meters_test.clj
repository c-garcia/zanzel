(ns zanzel.meters-test
  (:require [clojure.test :refer :all]
            [zanzel.meters :refer :all]))

(deftest defcounter-test
  (testing "When creating a counter"
    (do
      (defcounter c)
      (is (counter? c) "the counter is created")
      (ns-unmap *ns* 'c))))

(deftest counter-inc
  (testing "Given a created counter"
    (with-meters [c (counter-value 0)]
                 (counter-inc c)
                 (= 1 (counter-get c)))))
