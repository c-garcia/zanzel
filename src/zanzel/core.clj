(ns zanzel.core
  (:require [zanzel.search :as zse]
            [zanzel.system :as zsy]
            [zanzel.platform :as zsp]
            [zanzel.platform :as zpl]
            [zanzel.presentation :as zp]
            [zanzel.meters :as m])
  (:import (org.jfree.chart ChartFrame)))

(m/defcounter *explored-by-fn*)
(m/defcounter *generated-by-fn*)

(defn good-platform?-make
  "Returns a function that determines is a platform is a solution of the
  problem. A platform is selectable if
  * The platform capacity meets the required capacity
  * All the sytems in the platform are valid"
  [capacity-requirements]
  (fn [pl]
    (let [pl-capacity (zsp/platform-get-capacity pl)]
      (m/inc-counter *explored-by-fn*)
      (every? (fn [[k v]] (>= (get pl-capacity k 0) v)) capacity-requirements))))

(defn exhaustive-next-platforms
  "generates a sequence of next possible platforms"
  [curr-platform]
  ;; add a shelf to any of the existing systems, when possible
  ;; add one system, if it makes sense
  ;; all solutions
  (lazy-cat
    (for [sys (seq curr-platform)
          shelf-type (list :standard-shelf :replica-shelf :premium-shelf)
          :let [shelf (zsy/shelf-make shelf-type)]
          stack (zsy/system-find-suitable-stack sys shelf)
          :let [new-sys (zsy/system-add-shelf sys stack shelf)]]
      (-> curr-platform (disj sys) (conj new-sys)))
    (map #(conj curr-platform (zsy/storage-system-make %)) (list :standard-first :premium-first :replica-first))))

(defn directed-next-platforms-fn-make
  "Creates a function that given the current requirements, will add shelfs if there is a lack of capacity
  for the type the shelf provides or heads if there are no empty heads of the same type. This will miss solutions
  where we add heads where is still room to grow in the current ones"
  [reqs]
  (let [missig-cap-shelf-opts {:standard-size [:standard-shelf]
                               :premium-size  [:premium-shelf]
                               :replica-size  [:replica-shelf]}
        missing-cap-head-opts {:standard-size [:standard-first]
                               :premium-size  [:premium-first]
                               :replica-size  [:replica-first]}
        cap-factors (list :standard-size :premium-size :replica-size)]
    (fn [curr-plat]
      (let [curr-cap (zpl/platform-get-capacity curr-plat)]
        (lazy-cat
          (for [factor cap-factors
                :while (> (get reqs factor 0) (get curr-plat factor 0))
                shelf-opts (get missig-cap-shelf-opts factor)
                :let [shelf (zsy/shelf-make shelf-opts)]
                sys (seq curr-plat)
                stack (zsy/system-find-suitable-stack sys shelf)
                :let [new-sys (zsy/system-add-shelf sys stack shelf)]]
            (do
              (m/inc-counter *generated-by-fn*)
              (-> curr-plat (disj sys) (conj new-sys))))
          (for [factor cap-factors
                :while (> (get reqs factor 0) (get curr-plat factor 0))
                system-opts (get missing-cap-head-opts factor)
                :let [new-sys (zsy/storage-system-make system-opts)]
                :when (not (contains? curr-plat new-sys))]
            (do
              (m/inc-counter *generated-by-fn*)
              (conj curr-plat new-sys))))))))

(defn size-distance
  "calculates the distance of the current platform to the requirements
  only the factors in reqs are taken into account. It should not be very
  good as an heuristic since we can add capacity only in blocks.
  If we can add only in 4TB pieces and we require 1TB, the solution of adding
  a 4TB block makes us further than before"
  [pl reqs]
  (Math/sqrt
    (reduce
      (fn [acc [k v]] (+ acc (Math/pow (- (get pl k 0) v) 2)))
      0.0 reqs)))

(defn pruning-next-platforms-fn-make
  "Creates a function which prunes the possible solutions based on the results
  of a function. This function receives as arguments the current assessed solution
  and the one which has generated. If it returns truthy, the latter is kept."
  [pruning-fn]
  (fn [curr-platform]
    (->> curr-platform
         exhaustive-next-platforms
         (filter #(pruning-fn curr-platform %)))))

(defn find-configurations
  ([initial-platform capacity-requirements]
   (zse/bfs
     (list initial-platform)
     (good-platform?-make capacity-requirements)
     (directed-next-platforms-fn-make capacity-requirements)))
  ;;exhaustive-next-platforms))
  ([initial-platform capacity-requirements pruning-fn]
   (let [pruning-next-platforms-fn (pruning-next-platforms-fn-make pruning-fn)
         good-platforms-fn (good-platform?-make capacity-requirements)]
     (zse/bfs
       (list initial-platform)
       good-platforms-fn
       pruning-next-platforms-fn))))

(defn entry-point
  [curr-platform reqs target-dir num-solutions & {:keys [monitor] :or {monitor true}}]
  (binding [*explored-by-fn* (m/counter-starting-at 0)
            *generated-by-fn* (m/counter-starting-at 0)]
    (let [solutions (take num-solutions (find-configurations curr-platform reqs))
          idx-solutions (map vector (iterate inc 0) solutions)]
      (when monitor
        (let [{chart :chart ds-gen :generated ds-exp :explored} (zp/monitor-chart)
              start-time (System/currentTimeMillis)]
          (add-watch *explored-by-fn* :exp-update (fn [_ _ _ n]
                                              (.add ds-exp (- (System/currentTimeMillis) start-time) n)))
          (add-watch *generated-by-fn* :gen-update (fn [_ _ _ n]
                                               (.add ds-gen (- (System/currentTimeMillis) start-time) n)))
          (doto (ChartFrame. "Exploration status" chart) (.pack) (.setVisible true))))
      (doseq [[idx plat] idx-solutions]
        (zp/solution-as-png-file (format "%s/solution-%05d.png" target-dir idx) plat)))))
