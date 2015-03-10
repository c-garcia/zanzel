(ns zanzel.search
  (:require [zanzel.queues :as q]
            [clojure.set :as cs]))

(def ^:dynamic *visited*)
(def ^:dynamic *selected*)

(defmacro inc-counter
  [v]
  `(when (bound? (var ~v)) (send ~v inc)))

(defmacro add-to-counter
  [v i]
  `(when (bound? (var ~v)) (send ~v + ~i)))

(defn bfs
  "Starting from the xs sequence, collects those nodes satisfying select-node-fn (solutions)
  using a BFS strategy, next nodes are generated applying next-nodes-fn. next-nodes-nf is not
  applied to solutions (so they are not refined) unless refine-soltions is true."
  [xs select-node-fn next-nodes-fn & {:keys [refine-solutions] :or {refine-solutions false}}]
  (letfn [(generate-nodes
            [current-node]
            (next-nodes-fn current-node))
          (filter-non-visited-nodes
            [queue-nodes visited-nodes generated-nodes]
            (when-let [gn generated-nodes]
              (cs/difference
                (set gn)
                (cs/union
                  (set queue-nodes)
                  (set visited-nodes)))))
          (step [q visited]
                (when-let [node (q/head q)]
                  (inc-counter *visited*)
                  (let [next-visited (conj visited node)]
                    (if (select-node-fn node)
                      (do
                        (inc-counter *selected*)
                        (if-not refine-solutions
                          (lazy-seq (cons node (step (q/deq q) next-visited)))
                          (let [next-nodes (->> (generate-nodes node) (filter-non-visited-nodes q visited))
                                next-q (reduce q/enq (q/deq q) next-nodes)
                                next-visited next-visited]
                            (lazy-seq (cons node (step next-q next-visited))))))
                      (let [next-nodes (->> (generate-nodes node) (filter-non-visited-nodes q visited))
                            next-q (reduce q/enq (q/deq q) next-nodes)
                            next-visited next-visited]
                        (lazy-seq (step next-q next-visited)))))))]
    (step (into (q/empty-queue) xs) #{})))
