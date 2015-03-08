# zanzel

Some clojure namespaces and tools to generate potential Storage Array configurations given target space requirements for
three storage service classes: _standard_, _premium_ and _replica_. This is *an experiment* to get familiarised
with the Clojure lazy constructions taking advantage of a similar project at work. It has no intent to be used in production
environments (even though it could be easily evolved to something _very useful_).

## The storage systems

The storage hardware in the code, NetApp FAS2254, has some capacity figures attached. These are not generic
ones but specific to the
project that motivated this exercise. Even more, they have been distorted to protect the innocent. As a
conclusion, if looking at the code someone takes any conclusion about the capacity of these systems as sold by the
vendor, he or she is making a big mistake besides not having a clue about how disk storage systems work.
You have been warned.

## Usage

The code is mostly there to be used from a REPL.

    ;;; Namespaces
    (require '[zanzel.platform :as zpl])
    (require '[zanzel.system :as zs])
    (require '[zanzel.presentation :as zp])
    (require '[zanzel.core :as zc])
    (require '[clojure.pprint :refer :all])
    (require '[analemma.xml :as axml])
    (require '[analemma.svg :as asvg])

    ;;; The current platform
    (def curr-plat
        (zpl/platform-make
            (->
                (zs/storage-system-make :standard-first)
                (zs/system-add-shelf 0 (zs/shelf-make :standard-shelf))
                (zs/system-add-shelf 0 (zs/shelf-make :standard-shelf))
                (zs/system-add-shelf 0 (zs/shelf-make :standard-shelf))
                (zs/system-add-shelf 1 (zs/shelf-make :premium-shelf))
                (zs/system-add-shelf 1 (zs/shelf-make :premium-shelf)))
            (->
                (zs/storage-system-make :replica-first)
                (zs/system-add-shelf 0 (zs/shelf-make :replica-shelf)))))

    ;;; The requirements

    (def reqs {:standard-size 100 :premium-size 50 :replica-size 150})

    ;;; The solutions (we take the 10 first)
    ;;; We can also see the number of generated configurations and the assessed them

    (def solutions (binding [zc/*explored* (agent 0) zc/*generated* (agent 0)]
        (let [sols (doall (take 10 (zc/find-configurations curr-plat reqs)))]
            (println "Generated configurations:" @zc/*generated*)
            (println "Explored configurations:" @zc/*explored*)
            sols)))


## TO-DO

A lot of things. Unfortunately, I have not a lot of time at the moment but it would be nice to:

* Heavily refactor the code. Now it is somewhat messy since I was in learning-mode. I am not a super-expert in
   Clojure.
* Better model the data (maybe using prismatic/schema?).
* Direct the search which now is exhaustive (and provides infinite results)
* Add more configuration options that could satisfy the same requirements (DS4486 shelves).
* Provide more capacity factors and make the system choose the optimal solution (trivial):
    * Power / Cooling utilization (cost).
    * Item cost.
    * Setup time (it is not the same to add a shelf to a system that to configure a new array).

## The name: zanzel

Zanzel Melancthones is one of the minor characters appearing in the book "Rhialto the marvellous" by my favourite
writer, Jack Vance.

## License

Copyright © 2015 FIXME

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.