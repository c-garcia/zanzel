(ns zanzel.queues
  (:import (clojure.lang PersistentQueue)))

(defn empty-queue
  []
  PersistentQueue/EMPTY)

(defn enq 
  [q x & xs]
  (apply conj q x xs))

(defn head 
  [q]
  (peek q))

(defn deq 
  [q]
  (pop q))
