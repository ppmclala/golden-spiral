(ns mandelbrot
  (:import [java.awt.image BufferedImage]
           [javax.imageio ImageIO]
           [java.io File]))

(def max-iterations 1000)

(defn rgba->argb-pixel [[r g b a]]
  (unchecked-int
   (bit-or (bit-shift-left (bit-and a 0xFF) 24)
           (bit-or (bit-shift-left (bit-and r 0xFF) 16)
                   (bit-or (bit-shift-left (bit-and g 0xFF) 8)
                           (bit-and b 0xFF))))))

(def MSET    (rgba->argb-pixel [0 0 0 255]))
(def NEAR    (rgba->argb-pixel [190 0 255 255]))
(def NEARER  (rgba->argb-pixel [255 255 0 255]))
(def NEAREST (rgba->argb-pixel [0 255 0 255]))
(def FAR     (rgba->argb-pixel [24 24 24 255]))

(defn ->color [num-escapes]
  (cond
    (= max-iterations num-escapes) MSET
    (< num-escapes (* max-iterations 0.1)) FAR
    (< num-escapes (* max-iterations 0.3)) NEAR
    (< num-escapes (* max-iterations 0.5)) NEARER
    :else NEAREST))

(defn scale [val max-pixel range-min range-max]
  (+ range-min (* (/ val max-pixel) (- range-max range-min))))

(defn calc-escapes [cx cy]
  (loop [x cx y cy i 1]
    (if
     (and (<= (+ (* x x) (* y y)) (* 2 2)) (< i max-iterations))
      (recur
       (+ (- (* x x) (* y y)) cx)
       (+ (* 2 x y) cy)
       (inc i))
      i)))

(defn m-set [w h]
  (for [x* (range w)
        y* (range h)]
    (calc-escapes
     (scale x* w -2.0 0.5)
     (scale y* h -1.25 1.25))))

(defn rgba->png! [w h png-path pixels]
  (let [canvas (BufferedImage. w h BufferedImage/TYPE_INT_ARGB)
        len (* w h)
        pixel-array (int-array len)
        data (vec pixels)]

    (loop [i 0]
      (when (< i len)
        (let [packed (nth data i)]
          (aset-int pixel-array i packed)
          (recur (inc i)))))

    (.setRGB canvas 0 0 w h pixel-array 0 w)

    (ImageIO/write canvas "png" (File. png-path))))

(defn mandelbrot->png [s]
  (->>
   (m-set s s)
   (map ->color)
   (rgba->png! s s (format "output/mset-%d.png" s))))

(defn -main [& args]
  (let [s (if (first args) (Integer/parseInt (first args)) 100)]
    (mandelbrot->png s)))

(comment

  (mandelbrot->png 2000)

  ;;
  )
