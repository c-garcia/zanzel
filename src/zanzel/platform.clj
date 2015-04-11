(ns zanzel.platform
  "Namespace which aggregates systems into platforms. It allows us to calculate the capacity of a number
  of systems together. This will be used to determine if a set of systems meets some capacity requirements.

  It is, as of now, implemented as a multi-set."
  (:require [multiset.core :as ms]
            [zanzel.system :as zsy]
            [clojure.string :refer [join]]))

;; We may need not to add anything here

(defn platform-make
  "Creates a platform from a number of systems."
  [& more]
  (apply ms/multiset more))

(defn platform-get-capacity
  "Gets the capacity of a platform."
  [pl]
  (let [capacity-per-system (map zsy/system-get-capacity (seq pl))]
    (apply merge-with + capacity-per-system)))
