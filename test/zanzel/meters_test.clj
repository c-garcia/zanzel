(ns zanzel.meters-test
  (:require [clojure.test :refer :all]
            [zanzel.meters :refer :all]))

(defcounter "Documented counter" c)
(defcounter d)
(defgauge "Documented gauge" g)
(defgauge h)

(deftest defcounter-test
  (testing "When creating a counter"
    (is (counter? c) "the counter is created"))
  (testing "When creating a documented counter"
    (is (contains? (meta (var c)) :doc) "The documentation is present"))
  (testing "When creating an undocumented counter"
    (is (not (contains? (meta (var d)) :doc)) "The documentation is not present")))

(deftest counter-inc-test
  (testing "Given a created counter"
    (testing "When I increment a counter"
      (do
        (with-meters [c (counter-starting-at 0)]
                     (inc-counter c)
                     (await-for 5000 c)
                     (is (= 1 @c)) "The counter is increased")))
    (testing "When I increment a counter by more than one unit"
      (do
        (with-meters [c (counter-starting-at 0)]
                     (inc-counter c 5)
                     (await-for 5000 c)
                     (is (= 5 @c)) "The conter is correctly increased")))
    (ns-unmap *ns* 'c)))

(deftest defgauge-test
  (testing "When creating a gauge"
    (do
      (is (gauge? g) "the gauge is created")
      (ns-unmap *ns* 'g))))

(deftest set-gauge-to-test
  (testing "Given an existing gauge")
  (testing "When I set the counter to a value"
    (do
      (with-meters [g (gauge-with-level 0)]
                   (set-gauge-to g 35)
                   (await-for 5000 g)
                   (is (= 35 @g) "The value is properly set")))))

(deftest find-meters-test
  (testing "Given some existing counters in a namespace"
    (testing "When I invoke find"
      (is (= #{'c 'd 'g 'h} (find-meters 'zanzel.meters-test)) "All meters are found"))))
