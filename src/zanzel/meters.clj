(ns zanzel.meters)

;;; Counters

(defmacro defcounter
  "Defines a counter var, not bound to any value"
  ([c]
   `(defcounter nil ~c))
  ([d c]
   `(do
      (intern *ns* (quote ~c))
      (.setDynamic (var ~c))
      (alter-meta! (var ~c) assoc :meter true :meter-type :counter ~@(when d [:doc d]))
      (var ~c))))

(defn counter-starting-at
  "Creates a counter starting at v"
  [v]
  (agent v))

(defmacro counter?
  [c]
  `(= :counter (:meter-type (meta (var ~c)))))

(defmacro inc-counter
  "Increments a counter"
  ([c]
   `(when (bound? (var ~c)) (send ~c inc)))
  ([c v]
   `(when (bound? (var ~c)) (send ~c + ~v))))

;;; Gauges

(defmacro defgauge
  "Defines a gauge var, not bound to any value"
  ([g]
   `(defgauge nil ~g))
  ([d g]
   `(do
      (intern *ns* (quote ~g))
      (.setDynamic (var ~g))
      (alter-meta! (var ~g) assoc :meter true :meter-type :gauge ~@(when d [:doc d]))
      (var ~g))))

(defn gauge-with-level
  "Returns a gauge with an specific level"
  [l]
  (agent l))

(defmacro gauge?
  "Checks if a symbol resolves to a gauge var"
  [g]
  `(= :gauge (:meter-type (meta (var ~g)))))

(defmacro set-gauge-to
  "Sets a bound gauge var to an specific level. If it is not bound, does nothing"
  [g v]
  `(when (bound? (var ~g)) (send ~g (fn [o#] ~v))))

;;; Meters

(defmacro with-meters
  [bindings & body]
  (assert (vector? bindings) "Bindings should be a vector")
  (assert (even? (count bindings)) "Bindings should have an even number of args")
  `(binding ~bindings ~@body))

(defn find-meters
  [ns]
  (->> ns
       ns-publics
       (filter (fn [[_ v]] (:meter (meta v))))
       (map (fn [[k v]] k))
       set))

