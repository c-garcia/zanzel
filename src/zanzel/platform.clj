(ns zanzel.platform
  (:require [multiset.core :as ms]
            [zanzel.system :as zsy]
            [clojure.string :refer [join]]))

;; We may need not to add anything here

(defn platform-make
  [& more]
  (apply ms/multiset more))

(defn platform-get-capacity
  [pl]
  (let [capacity-per-system (map zsy/system-get-capacity (seq pl))]
    (apply merge-with + capacity-per-system)))
