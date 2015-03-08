(ns zanzel.core-test
  (:require [clojure.test :refer :all]
            [zanzel.core :refer :all]
            [zanzel.platform :refer :all]
            [zanzel.system :refer :all]
            [zanzel.system :as zsy]))


;; REPL code to paste
;; (require '[zanzel.platform :refer :all] '[zanzel.system :refer :all] '[zanzel.core :refer :all] :reload)
;; (def data-system (-> (storage-system-make :premium-first) (system-add-shelf 1 (shelf-make :standard-shelf))))
;; (def replica-system (-> (storage-system-make :replica-first) (system-add-shelf 1 (shelf-make :replica-shelf))))
;; (def initial-platform (platform-make data-system replica-system))
;; (def target-platform (platform-make (-> data-system (system-add-shelf 0 (shelf-make :premium-shelf))) replica-system))

(deftest search-simple
  (testing "Given an initial platform with two systems, one for data, another for replica with P:4, S: 12, R: 30"
    (let [data-system (-> (storage-system-make :premium-first) (system-add-shelf 1 (shelf-make :standard-shelf)))
          replica-system (-> (storage-system-make :replica-first) (system-add-shelf 1 (shelf-make :replica-shelf)))
          initial-platform (platform-make data-system replica-system)]
      (testing "Given capacity requirements beyond those offered by the platform"
        (testing "In a single factor: P: 8"
          (let [premium-capacity 8
                standard-capacity 12
                replica-capacity (+ standard-capacity premium-capacity)
                required-capacity {:standard-size standard-capacity,
                                   :premium-size  premium-capacity,
                                   :replica-size  replica-capacity}
                target-platform (platform-make (-> data-system (system-add-shelf 0 (shelf-make :premium-shelf))) replica-system)]
            (testing "When I invoke find-configurations"
              (let [solutions (find-configurations initial-platform required-capacity)]
                (is
                  (some (partial = target-platform) solutions)
                  "Then the target configuration is among the found solutions")))))
        (testing "In two factors, P: 6, S: 14"
          (let [premium-capacity 8
                standard-capacity 30
                replica-capacity (+ standard-capacity premium-capacity)
                required-capacity {:standard-size standard-capacity,
                                   :premium-size  premium-capacity,
                                   :replica-size  replica-capacity}
                target-platform (platform-make (-> data-system
                                                 (system-add-shelf 0 (shelf-make :premium-shelf))
                                                 (system-add-shelf 1 (shelf-make :standard-shelf)))
                                             replica-system)]
            (testing "When I invoke find-configurations"
              (let [solutions (find-configurations initial-platform required-capacity)]
                (is
                  (some (partial = target-platform) solutions)
                  "Then the target configuration is among the found solutions")))))))))

(deftest good-platform?-make-test
  (testing "Given an empty platform"
    (let [pl (platform-make)]
      (testing "and requirements for zero capacity"
        (let [good-for? (good-platform?-make {})]
          (is (good-for? pl) "the platform can meet the requirements")))
      (testing "and requirements for capacity bigger than zero"
        (let [good-for? (good-platform?-make {:premium-size 1})]
          (is (not (good-for? pl)) "the platform cannot meet the requirements")))))
  (testing "Given a platform with a single system with a single capacity type"
    (let [pl (platform-make (zsy/storage-system-make :standard-first))]
      (testing "and requirements for zero capacity"
        (let [good-for? (good-platform?-make {})]
          (is (good-for? pl) "the platform can meet the requirements")))
      (testing "and capacity requirements for the same factor in an smaller amount than offered"
        (let [good-for? (good-platform?-make {:standard-size 1})]
          (is (good-for? pl) "the platform can meet the requirements")))
      (testing "and capacity requirements for the same factor in a greater amount than offered"
        (let [good-for? (good-platform?-make {:standard-size 100})]
          (is (not (good-for? pl)) "the platform cannot meet the requirements")))
      (testing "and capacity requirements achievable for the offered factor but with another, non-offered factor"
        (let [good-for? (good-platform?-make {:standard-size 1 :premium-size 2})]
          (is (not (good-for? pl)) "the plaform cannot meet the requirements")))
      (testing "and capacity requirements only for factors not offered by the platform"
        (let [good-for? (good-platform?-make {:other-size 1})]
          (is (not (good-for? pl)) "the platform cannot meet the requirements")))))
  (testing "Given a platform with multiple systems and multiple capacity factors"
    (let [sys1 (zsy/storage-system-make :premium-first)
          sys2 (zsy/storage-system-make :standard-first)
          pl (platform-make sys1 sys2)]
      (testing "and requirements for the same capacity the platform offers"
        (let [pl-cap (platform-get-capacity pl)
              good-for? (good-platform?-make pl-cap)]
          (is (good-for? pl) "the platform meets the requirements"))))))

(deftest next-platforms-test
  (testing "Given a platform with a single system"
    (let [ssys1 (storage-system-make :standard-first)
          pl (platform-make ssys1)]
      (testing "When I invoke the operation"
        (let [next-pls (set (exhaustive-next-platforms pl))]
          (testing "Systems without external shelves have been added"
            (is (contains? next-pls
                           (platform-make
                             ssys1
                             (storage-system-make :standard-first))) "standard first")
            (is (contains? next-pls
                           (platform-make
                             ssys1
                             (storage-system-make :premium-first))) "premium first")
            (is (contains? next-pls
                           (platform-make
                             ssys1
                             (storage-system-make :replica-first))) "replica first"))
          (testing "Suitable shelves have been added to the existing system"
            (is (contains? next-pls
                           (platform-make
                             (system-add-shelf ssys1 0 (shelf-make :standard-shelf)))) "same stack, same shelf type")
            (is (contains? next-pls
                           (platform-make
                             (system-add-shelf ssys1 1 (shelf-make :standard-shelf)))) "next stack, same shelf type")
            (is (contains? next-pls
                           (platform-make
                             (system-add-shelf ssys1 1 (shelf-make :premium-shelf)))) "next stack, alternative shelf type")
            (is (contains? next-pls
                           (platform-make
                             (system-add-shelf ssys1 1 (shelf-make :replica-shelf)))) "next stack, alternative shelf type")))))))
