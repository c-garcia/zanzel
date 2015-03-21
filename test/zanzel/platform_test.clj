(ns zanzel.platform-test
  (:require [clojure.test :refer :all]
            [zanzel.platform :refer :all]
            [zanzel.system :as zsy]))


(deftest platform-get-capacity-test
  (testing "Given an empty platform"
    (let [pl (platform-make)]
      (testing "When I invoke the operation"
        (is (empty? pl) "The capacity shows no factors, values"))))
  (testing "Givem a platform with a single system"
    (let [sys (zsy/storage-system-make :premium-first)
          pl (platform-make sys)]
      (testing "When I invoke the operation"
        (is (=
              (zsy/system-get-capacity sys)
              (platform-get-capacity pl))
            "The capacity of the platform is the same as that one of the system"))))
  (testing "Given a platform with multiple systems"
    (let [sys1 (zsy/storage-system-make :standard-first)
          sys2 (zsy/storage-system-make :premium-first)
          pl (platform-make sys1 sys2)]
      (is (=
            {:standard-size 19 :premium-size 6 :ops (+ 570 870) :disks 48 :rack-u 6}
            (platform-get-capacity pl))))))
