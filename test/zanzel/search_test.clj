(ns zanzel.search-test
  (:require [clojure.test :refer :all]
            [zanzel.search :refer :all]))

(deftest bfs-test
  (testing "No refine solutions"
    (testing "Given an initial sequence of [1] a next-nodes-fn of [(inc x) if x  < 100] and a select-node-fn of % mod 17"
      (testing "When I invoke bfs"
        (let [res (bfs (list 1) #(== (mod % 17) 0) (fn [x] (if (< x 100) [(inc x) (* 2 x)] nil)))]
          (is (= #{17 34 51 68 85} (set res)) "I get the multiples of 17 below 100"))))
    (testing "Given an initial sequence of [1] and next-nodes-fn of [(inc x)] and a select-node-fn of true"
      (testing "When I invoke bfs"
        (let [res (bfs (list 1) (fn [x] true) (fn [x] [(inc x)]))]
          (is (= '(1) res) "Generates a list with the first solution only")))))
  (testing "Refine solutions"
    (testing "Given an initial sequence of [1] and next-nodes-fn of [(inc x)] and a select-node-fn of true"
      (testing "When I invoke bfs"
        (let [res (take 20 (bfs (list 1) (fn [x] true) (fn [x] [(inc x)]) :refine-solutions true))]
          (is (= (range 1 21) res) "Obtains solutions traversing from solutions"))))))
