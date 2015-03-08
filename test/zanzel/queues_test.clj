(ns zanzel.queues-test
  (:require [clojure.test :refer :all]
            [zanzel.queues :refer :all]))

(deftest empty-queue-test
  (testing "Given a new queue"
    (testing "When I create it"
      (is (empty? (empty-queue)) "is empty"))
    (testing "When I add a single element"
      (let [q (-> (empty-queue) (enq 1))]
        (is (= 1 (head q)) "appears at the head")))
    (testing "When try to get the head"
      (let [q (empty-queue)]
        (is (nil? (head q)) "it returns nil")))))
