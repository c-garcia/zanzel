(ns zanzel.meters)

(defmacro defcounter
  [c]
  `(do
     (def ~c)
     (alter-meta! (var ~c) assoc :dynamic true :meter true :meter-type :counter)
     (var ~c)))

(defmacro counter?
  [c]
  `(= :counter (:meter-type (meta (var ~c)))))

(defmacro inc-counter
  [v]
  `(when (bound? (var ~v)) (send ~v inc)))

(defmacro add-to-counter
  [v i]
  `(when (bound? (var ~v)) (send ~v + ~i)))
