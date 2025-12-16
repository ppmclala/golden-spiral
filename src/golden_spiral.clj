(ns golden-spiral
  (:require [hiccup2.core :refer [html]]))

(def canvas-width 1800)
(def canvas-height 1200)
(def factor 10)

(defn fib [n]
  (if (<= n 1) 1 (+ (fib (- n 2)) (fib (- n 1)))))

(def fast-fib (memoize fib))

(defn fibs [n]
  (->> (range n) (map fast-fib)))

(defn canvas-center []
  [(int (/ canvas-width 2)) (int (/ canvas-height 2))])

(def center-x (first (canvas-center)))
(def center-y (last (canvas-center)))

(defn ->color [n]
  (let [palette ["#ff6b6b" "#ff9f1c"
                 "#ffd93d" "#aacc00"
                 "#6bcb77" "#4ecdc4"
                 "#4d96ff" "#9b5de5"
                 "#f15bb5" "#00bbf9"]]
    (nth palette (mod n (count palette)))))

(defn rect->svg [i r]
  [:rect (merge r {:fill (->color i)})])

(defn arc-dir [i]
  (condp = (rem i 4)
    0 :bottom-right->top-left
    1 :top-right->bottom-left
    2 :top-left->bottom-right
    3 :bottom-left->top-right))

(defn rect->arc [i {:keys [x y height width]}]
  (let [data
        (case (arc-dir i)
          :bottom-right->top-left
          (str "M " (+ x width) " " (+ y height) " A " width " " width " 0 0 0 " x " " y)

          :top-right->bottom-left
          (str "M " (+ x width) " " y " A " width " " width " 0 0 0 " x " " (+ y height))

          :top-left->bottom-right
          (str "M " x " " y " A " width " " width " 0 0 0 " (+ x width) " " (+ y height))

          :bottom-left->top-right
          (str "M " x " " (+ y  height) " A " width " " width " 0 0 0 " (+ x width) " " y))]

    [:path {:d data :stroke "white" :stroke-width "3" :fill "none" :i i}]))

(defn ->rect [{:keys [x y width height]} i f]
  (let [size (* f factor)
        make-rect #(merge {:width size :height size :i i} %)]

    (case (arc-dir i)
      :bottom-right->top-left
      (let [tr-x (+ x width) tr-y y]
        (make-rect {:x (- tr-x size) :y (- tr-y size)}))

      :top-right->bottom-left
      (let [tl-x x tl-y y]
        (make-rect  {:x (- tl-x size) :y tl-y}))

      :top-left->bottom-right
      (let [bl-x x bl-y (+ y height)]
        (make-rect {:x bl-x :y bl-y}))

      :bottom-left->top-right
      (let [br-x (+ x width) br-y (+ y height)]
        (make-rect {:x br-x :y (- br-y size)})))))

(defn ->rects [fibs]
  (let [ifibs (map-indexed (fn [i f] [i f]) fibs)]
    (reduce
     (fn [rects [i f]]
       (conj rects
             (if (= (count rects) 0)
               {:i i :x center-x :y center-y :height (* 1 factor) :width (* 1 factor)}
               (->rect (last rects) i f))))
     []
     ifibs)))

(defn- rect->text [_ {:keys [x y width height]}]
  [:text {:x (+ (- x 3) (/ width 2))
          :y (+ y 3 (/ height 2))
          :font-family "Arial"
          :font-size "16"
          :fill "black"}
   (quot height factor)])

(defn rect->layout [rs]
  (let [orientation
        (if (odd? (count rs)) :portrait :landscape)

        top-left-pos
        (reduce
         (fn [[x-min y-min] {:keys [x y]}]
           (if (and (<= x x-min) (<= y y-min))
             [x y]
             [x-min y-min]))
         [canvas-width canvas-height]
         rs)

        last-2
        (->> rs reverse (take 2) (map :height))

        max-side
        (->> last-2 (reduce +))

        min-side
        (first last-2)

        layout
        {:x-offset (first top-left-pos)
         :y-offset (last top-left-pos)}]

    (case orientation
      :portrait (merge layout {:height max-side :width min-side})
      :landscape (merge layout {:height min-side :width max-side}))))

(defn translate-rect [layout r]
  (-> r
      (update :x - (:x-offset layout))
      (update :y - (:y-offset layout))))

(defn -main [& args]
  (let [n (if (first args) (Integer/parseInt (first args)) 10)
        rs (->> (fibs n) ->rects)
        canvas-layout (rect->layout rs)
        rs* (map (partial translate-rect canvas-layout) rs)
        rs-svg (map-indexed rect->svg rs*)
        arcs-svg (map-indexed rect->arc rs*)
        txt-svg (map-indexed rect->text rs*)
        svg-content
        (into
         [:svg {:xmlns "http://www.w3.org/2000/svg"
                :width (:width canvas-layout)
                :height (:height canvas-layout)
                :viewBox (str "0 0 " (:width canvas-layout) " " (:height canvas-layout))}]
         (concat rs-svg arcs-svg txt-svg))]

    (spit (str "output/golden_spiral-" n ".svg") (html svg-content))
    (println (str "Generated golden_spiral.svg with " n " iterations."))))
