(ns zanzel.system
  (:require [clojure.string :refer [upper-case join escape]]
            [analemma.svg :as svg]))

;; Capacity parameters
;;
;; Heads will have
;; 24 - (3+3+2) = 18 useful disks
;; 18 - 4 RAID = 12 data
;; Premium-first = 12 * 0.5 = 6TB / 870
;; Standard-first = 12 * 1.6 = 19 / 570
;; Replica-first = 12 * 2.4 = 28 / 420
;;
;; Shelves
;; 22 + spare, RG:9+2
;; Premium: 18*0.5 = 9TB / 1305
;; Standard: 18 * 1.6 = 29 / 870
;; Replica: 18 * 2.4 = 43 / 645
;;

(defn storage-system-make
  [sys-class]
  (case sys-class
    :premium-first {:type          :fas2552,
                    :max-disks     144,
                    :max-ops-stack 6000,
                    :stacks        [
                                    [{:location :internal, :disk-type :sas600gb, :stack 0
                                      :capacity {:premium-size 6, :ops (* 6 145), :disks 24 :rack-u 2}}] []]}
    :standard-first {:type          :fas2554,
                     :max-disks     144,
                     :max-ops-stack 6000,
                     :stacks        [
                                     [{:location :internal, :disk-type :sata2tb,, :stack 0
                                       :capacity {:standard-size 19, :ops (* 19 30), :disks 24 :rack-u 4}}] []]}
    :replica-first {:type          :fas2554,
                    :max-disks     144,
                    :max-ops-stack 6000,
                    :stacks        [
                                    [{:location :internal :disk-type :sata3tb,, :stack 0
                                      :capacity {:replica-size 28, :ops (* 28 15), :disks 24 :rack-u 4}}] []]}))

(defn shelf-make
  [shelf-class]
  (case shelf-class
    :premium-shelf {:location  :shelf
                    :type      :ds2246
                    :disk-type :sas600gb
                    :capacity  {:premium-size 9, :ops (* 9 145), :disks 24 :rack-u 2}}
    :standard-shelf {:location  :shelf
                     :type      :ds4246
                     :disk-type :sata2tb
                     :capacity  {:standard-size 29, :ops (* 29 30), :disks 24 :rack-u 4}}
    :replica-shelf {:location  :shelf
                    :type      :ds4246
                    :disk-type :sata3tb
                    :capacity  {:replica-size 43, :ops (* 43 15), :disks 24 :rack-u 4}}))

(defn system-get-stack
  "Gets the stack 0 or 1 from the system"
  [storage-system stack-number]
  (get-in storage-system [:stacks stack-number]))

(defn stack-get-ops
  "Gets the maximum ops that a stack can support"
  [stack]
  (reduce (fn [acc item] (+ acc (get-in item [:capacity :ops]))) 0 stack))

(defn stack-get-disk-type
  "Gets the stack disk type"
  [stack]
  (when-let [fi (first stack)]
    (:disk-type fi)))

(defn system-find-suitable-stack-old
  "Returns a pair [index stack] in which we could add a shelf
  or nil"
  [system shelf]
  (let [shelf-disk-type (:disk-type shelf)
        stacks-with-idx (map vector (iterate inc 0) (:stacks system))
        idx (seq (filter (fn [[i s]] (or (empty? s) (= shelf-disk-type (stack-get-disk-type s)))) stacks-with-idx))]
    (if-not (nil? idx) (first idx))))

(defn system-add-shelf
  "Adds a shelf to a system in the selected stack"
  [system stack-idx shelf]
  (let [added-shelf (assoc shelf :stack stack-idx)]
    (update-in system [:stacks stack-idx] conj added-shelf)))

(defn stack-get-capacity
  [stack]
  (let [cap-entries (map :capacity stack)]
    (apply merge-with + cap-entries)))

(defn system-get-capacity-per-stack
  [system]
  (let [cap-summaries (map stack-get-capacity (:stacks system))]
    cap-summaries))

(defn system-get-internal-shelves
  "Gets the internal shelf"
  [system]
  (->> (get-in system [:stacks 0] [])
       (filter #(= :internal (:location %)))))

(defn system-get-stack-shelves
  "Gets only external shelves belonging to a stack"
  [system stack-idx]
  (->> (get-in system [:stacks stack-idx] [])
       (filter #(= :shelf (:location %)))))

(defn system-get-capacity
  [system]
  (apply merge-with + (system-get-capacity-per-stack system)))

(defn system-find-suitable-stack
  "Returns an ordered vector of the suitable stacks to add a shelf"
  [system shelf]
  (letfn [(max-disks-reached-after-upgrade
            []
            (let [total-cap (system-get-capacity system)
                  upgd-disks-cap (+ (get total-cap :disks 0) (get-in shelf [:capacity :disks] 0))]
              (> upgd-disks-cap (get system :max-disks))))
          (stack-valid-disk-type
            [stack-idx]
            (=
              (get-in system [:stacks stack-idx 0 :disk-type] (:disk-type shelf))
              (:disk-type shelf)))
          (stack-max-ops-reached-after-upgrade
            [stack-idx]
            (> (+
                 (get (stack-get-capacity (get-in system [:stacks stack-idx])) :ops 0)
                 (get-in shelf [:capacity :ops] 0))
               (get-in system [:max-ops-stack] 0)))]
    (if-not (max-disks-reached-after-upgrade)
      (vec (filter #(and
                     (stack-valid-disk-type %1)
                     (not (stack-max-ops-reached-after-upgrade %1)))
                   (range (count (:stacks system)))))
      [])))


