(ns zanzel.system-test
  (:require [clojure.test :refer :all]
            [zanzel.system :refer :all]))

(deftest system-test
  (testing "when I create a storage system"
    (testing "the type is set correctly"
      (are [ss-class stype] (= stype (:type (storage-system-make ss-class)))
                            :standard-first :fas2554
                            :premium-first :fas2552
                            :replica-first :fas2554))
    (testing "it has the correct stacks number"
      (are [ss-class stacks-num] (= stacks-num (count (:stacks (storage-system-make ss-class))))
                                 :standard-first 2
                                 :premium-first 2
                                 :replica-first 2))))

(deftest shelf-test
  (testing "when I create a shelf"
    (testing "the type is set correctly"
      (are [sh-class sh-type] (= sh-type (:type (shelf-make sh-class)))
                              :standard-shelf :ds4246
                              :replica-shelf :ds4246
                              :premium-shelf :ds2246))
    (testing "the disk-type is set correctly"
      (are [sh-class ds-type] (= ds-type (:disk-type (shelf-make sh-class)))
                              :standard-shelf :sata2tb
                              :replica-shelf :sata3tb
                              :premium-shelf :sas600gb))))

(deftest system-add-shelf-test
  (testing "Given an existing system"
    (let [system (storage-system-make :standard-first)]
      (testing "when I add a shelf to the stack 0"
        (let [shelf (shelf-make :standard-shelf)
              new-system (system-add-shelf system 0 shelf)]
          (is (= 2 (count (system-get-stack new-system 0))) "There are two shelves in the stack 0")
          (is (= 0 (count (system-get-stack new-system 1))) "There is no shelf in the stack 1")
          (is (some (fn [x] (= (:location x) :internal)) (system-get-stack new-system 0)) "There is an internal shelf in the stack 0")
          (is (some (fn [x] (= (:location x) :shelf)) (system-get-stack new-system 0)) "There is a normal shelf in the stack 0")))
      (testing "when I add a shelf to the stack 1"
        (let [shelf (shelf-make :standard-shelf)
              new-system (system-add-shelf system 1 shelf)]
          (is (= 1 (count (system-get-stack new-system 0))) "There is one shelf in stack 0")
          (is (= 1 (count (system-get-stack new-system 1))) "There is one shelf in stack 1")
          (is (some (fn [x] (= (:location x) :internal)) (system-get-stack new-system 0)) "The shelf in the stack 0 is internal")
          (is (some (fn [x] (= (:location x) :shelf)) (system-get-stack new-system 1)) "The shelf in the stack 1 is a normal shelf"))))))

(deftest system-get-capacity-test
  (testing "Given an existing system with no shelves"
    (testing "When I invoke system-get-capacity, the capacity is correct"
      (are [system cap] (= cap (system-get-capacity system))
                        (storage-system-make :standard-first) {:standard-size 19 :ops (* 19 30) :disks 24 :rack-u 4}
                        (storage-system-make :premium-first) {:premium-size 6 :ops (* 6 145) :disks 24 :rack-u 2}
                        (storage-system-make :replica-first) {:replica-size 28 :ops (* 28 15) :disks 24 :rack-u 4})))
  (testing "Given an existing systems with shelves"
    (testing "When I invoke system-get-capacity, the capacity is correct"
      (are [system cap] (= cap (system-get-capacity system))
                        (-> (storage-system-make :standard-first) (system-add-shelf 1 (shelf-make :premium-shelf)))
                        {:standard-size 19 :premium-size 9 :ops (+ 570 1305) :disks 48 :rack-u 6}
                        (-> (storage-system-make :premium-first) (system-add-shelf 1 (shelf-make :replica-shelf)))
                        {:replica-size 43 :premium-size 6 :ops (+ 870 645) :disks 48 :rack-u 6}))))

(deftest system-find-suitable-stack-test
  (testing "Given an existing system and a shelf"
    (testing "When I invoke operation"
      (testing "It finds the correct stacks in which the shelf could be placed"
        (are [sys shelf stacks] (= stacks (system-find-suitable-stack sys shelf))
                                ;; both stacks are suitable
                                (storage-system-make :standard-first)
                                (shelf-make :standard-shelf)
                                [0 1]
                                ;; one stack has already a disk type
                                (storage-system-make :standard-first)
                                (shelf-make :premium-shelf)
                                [1]
                                ;; max disks reached
                                (->
                                  (storage-system-make :standard-first)
                                  (system-add-shelf 0 (shelf-make :standard-shelf))
                                  (system-add-shelf 0 (shelf-make :standard-shelf))
                                  (system-add-shelf 0 (shelf-make :standard-shelf))
                                  (system-add-shelf 1 (shelf-make :premium-shelf))
                                  (system-add-shelf 1 (shelf-make :premium-shelf)))
                                (shelf-make :standard-shelf)
                                []
                                ;; max IOPS would be surpassed in one stack, the other is suitable
                                (->
                                  (storage-system-make :premium-first)
                                  (system-add-shelf 0 (shelf-make :premium-shelf))
                                  (system-add-shelf 0 (shelf-make :premium-shelf))
                                  (system-add-shelf 0 (shelf-make :premium-shelf)))
                                (shelf-make :premium-shelf)
                                [1]
                                ;; max IOPS would be surpassed in one stack, the other is using other disk technology
                                (->
                                  (storage-system-make :premium-first)
                                  (system-add-shelf 0 (shelf-make :premium-shelf))
                                  (system-add-shelf 0 (shelf-make :premium-shelf))
                                  (system-add-shelf 0 (shelf-make :premium-shelf))
                                  (system-add-shelf 1 (shelf-make :standard-shelf)))
                                (shelf-make :premium-shelf)
                                [])))))