(ns zanzel.presentation
  (:require [zanzel.system :as zsy]
            [zanzel.platform :as zpl]
            [clojure.string :refer [join upper-case escape]]
            [analemma.xml :as axml]
            [analemma.svg :as svg]
            [tikkba.transcoder :as t]
            [tikkba.dom :refer [svg-doc]])

  (:import (org.jfree.chart ChartFactory LegendItem LegendItemCollection)
           (org.jfree.data.xy XYSeriesCollection XYSeries)
           (org.jfree.chart.plot PlotOrientation)))

(defn- head-as-str
  [system]
  (let [internal-disks (first (filter #(= :internal (:location %)) (get-in system [:stacks 0])))
        stype (upper-case (name (:type system)))
        dtype (upper-case (name (:disk-type internal-disks)))]
    (format "%s:%s" stype dtype)))

(defn- shelf-as-str
  [shelf]
  (let [stype (upper-case (name (:type shelf)))
        sdisk (upper-case (name (:disk-type shelf)))]
    (format "%s:%s" stype sdisk)))

(defn- storage-system-as-str
  [system]
  (let [head-str (head-as-str system)
        stack0-shelves (filter #(= :shelf (:location %)) (get-in system [:stacks 0]))
        stack1-shelves (filter #(= :shelf (:location %)) (get-in system [:stacks 1]))
        stack0-str (->> stack0-shelves
                        (map shelf-as-str)
                        (join "->"))
        stack1-str (->> stack1-shelves
                        (map shelf-as-str)
                        (join "->"))
        res (format "%s->%s\n\t+->%s" head-str stack0-str stack1-str)]
    res))

(defn- html-escape
  [s]
  (escape s {\< "&lt;", \> "&gt;", \& "&amp;"}))

(defn svg-block
  [x y w h fs txt & {:keys [color text-color] :or {color "black" text-color "white"}}]
  (let [text-x (+ x (int (/ w 2)))
        text-y (+ y (int (/ h 2)))]
    (svg/group
      (->
        (svg/rect x y h w)
        (svg/add-style :fill color :stroke "black" :stroke-width 2))
      (->
        (svg/text {:x text-x :y text-y :text-anchor "middle"} (html-escape txt))
        (svg/add-style :font-family "monospace" :font-size fs :fill text-color)))))

(defn head-as-svg
  [system & {:keys [color text-color] :or {color "black" text-color "white"}}]
  (let [internal-shelf (first (filter #(= :internal (get % :location)) (zsy/system-get-internal-shelves system)))
        height (* 10 (get-in internal-shelf [:capacity :rack-u]))
        head-txt (-> system head-as-str html-escape)
        svg (svg-block 0 0
                       120 height
                       10 head-txt
                       :color color :text-color text-color)]
    [svg height]))

(defn storage-system-as-svg
  [system & {:keys [head-color head-text-color stack0-color stack0-text-color stack1-color stack1-text-color]
             :or   {head-color        "#626266"
                    head-text-color   "#FFFFFF"
                    stack0-color      "#cc9e61"
                    stack0-text-color "black"
                    stack1-color      "#938172"
                    stack1-text-color "black"}}]
  (let [stack-colors [stack0-color stack1-color]
        stack-text-colors [stack0-text-color stack1-text-color]
        stack0 (zsy/system-get-stack-shelves system 0)
        stack1 (zsy/system-get-stack-shelves system 1)
        [svg-head y-pos] (head-as-svg system :color head-color :text-color head-text-color)
        [svg-shelves y-pos] (reduce
                              (fn [[svgs y-pos] shelf]
                                (let [stack (get shelf :stack)
                                      rack-us (get-in shelf [:capacity :rack-u])
                                      height (* 10 rack-us)
                                      svg (svg-block
                                            0 y-pos
                                            120 height
                                            10 (shelf-as-str shelf)
                                            :color (get stack-colors stack)
                                            :text-color (get stack-text-colors stack))
                                      new-y-pos (+ y-pos height)]
                                  [(conj svgs svg) new-y-pos]))
                              [[] y-pos]
                              (lazy-cat stack0 stack1))]
    [(apply svg/group
            svg-head
            svg-shelves) y-pos]))

(defn platform-as-svg
  [platform & {:keys [head-color head-text-color stack0-color stack0-text-color stack1-color stack1-text-color]
               :or   {head-color        "#626266"
                      head-text-color   "#FFFFFF"
                      stack0-color      "#cc9e61"
                      stack0-text-color "black"
                      stack1-color      "#938172"
                      stack1-text-color "black"}}]
  (let [[system-svgs max-x max-y] (reduce
                                    (fn [[svgs x-pos max-y-pos] sys]
                                      (let [[svg y-pos] (storage-system-as-svg sys
                                                                               :head-color head-color :head-text-color head-text-color
                                                                               :stack0-color stack0-color :stack0-text-color stack0-text-color
                                                                               :stack1-color stack1-color :stack1-text-color stack1-text-color)
                                            svg (svg/translate svg x-pos)
                                            new-x-pos (+ x-pos 140)
                                            new-y-pos (max y-pos max-y-pos)]
                                        [(conj svgs svg) new-x-pos new-y-pos]))
                                    [[] 0 0]
                                    (seq platform))]
    [(apply svg/group system-svgs) (- max-x 20) max-y]))

(defn grid-as-svg
  [width height u]
  (let [profile (svg/rect 0 0 height width :fill "none")
        horz (map #(svg/line 0 %1 width %1 :stroke "black") (range 0 height u))]
    (apply svg/group profile horz)))

(defn capacity-as-svg
  [width std prm rpl ops u]
  (let [std-txt (format "Std: %d TB." std)
        prm-txt (format "Prm: %d TB." prm)
        rpl-txt (format "Rpl: %d TB." rpl)
        ops-txt (format "Ops: %d." ops)
        u-txt (format "Rack-U: %d." u)
        x-pos (/ width 2)]
    (letfn [(text-item [x y text]
                       (-> (svg/text (html-escape text))
                           (axml/add-attrs :x x :y y :text-anchor "middle")
                           (svg/style :fill "white" :font-size "12px" :font-family "monospace" :alignment-baseline :middle)))]
      (svg/group
        (svg/rect 0 0 100 width :fill "black" :stroke "black")
        (text-item x-pos 10 std-txt)
        (text-item x-pos 30 prm-txt)
        (text-item x-pos 50 rpl-txt)
        (text-item x-pos 70 ops-txt)
        (text-item x-pos 90 u-txt)))))

(defn solution-as-svg
  [platform]
  (let [[plat-svg max-pl-x max-pl-y] (platform-as-svg platform)
        num-systems (count platform)
        pl-cap (zpl/platform-get-capacity platform)
        {standard :standard-size premium :premium-size replica :replica-size ru :rack-u ops :ops} pl-cap
        header-txt (format "Config (%d systems)" num-systems ru)
        header-svg (svg-block 0 0 max-pl-x 40 12 header-txt :color "#541F14" :text-color "white")
        footer-svg (capacity-as-svg max-pl-x standard premium replica ops ru)
        all-svg (svg/group
                  header-svg
                  (svg/translate (grid-as-svg max-pl-x max-pl-y 10) 0 40)
                  (svg/translate plat-svg 0 40)
                  (svg/translate footer-svg 0 (+ 40 max-pl-y)))]
    [all-svg max-pl-x (+ 40 100 max-pl-y)]))

(defn monitor-chart
  []
  (let [ds-gen (XYSeries. "generated")
        ds-exp (XYSeries. "explored")
        series-col (doto (XYSeriesCollection.) (.addSeries ds-gen) (.addSeries ds-exp))
        chart (ChartFactory/createXYLineChart
                      "Configurations"
                      "Time"
                      "Number"
                      series-col
                      PlotOrientation/VERTICAL
                      true false false)]
    {:chart chart :generated ds-gen :explored ds-exp}))

(defn solution-as-png-file
  [filename platform]
  (let [[svg-group width height] (solution-as-svg platform)
        canvas (svg-doc (svg/svg svg-group))]
    (t/to-png canvas filename)))