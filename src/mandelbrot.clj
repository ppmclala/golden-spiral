(ns mandelbrot
  (:require
   [clojure.core.async :refer [<! >! chan close! go go-loop pipeline]])
  (:import
   [java.awt.image BufferedImage]
   [java.io File]
   [javax.imageio ImageIO]))

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

(defn pm-set [s])

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

(defn calculate-pixel [x y n]
  (let [cx (scale x n -2.0 0.5)
        cy (scale y n -1.25 1.25)]
    (-> (calc-escapes cx cy) ->color)))

(defn process-tile! [array image-width tile-indices]
  (doseq [idx tile-indices]
    (let [x (rem idx image-width)
          y (quot idx image-width)
          color (calculate-pixel x y image-width)]
      (aset-int array idx color))))

(defn parallel-mset [n]
  (let [img-array (int-array (* n n))
        all-indices (range (* n n))
        tiles (partition-all 10000 all-indices)]

    (doall (pmap #(process-tile! img-array n %) tiles))

    img-array))

(defn pixels->png [sz png-path pixel-array]
  (let [canvas (BufferedImage. sz sz BufferedImage/TYPE_INT_ARGB)]
    (.setRGB canvas 0 0 sz sz pixel-array 0 sz)
    (ImageIO/write canvas "png" (File. png-path))))

(defn parallel-mandelbrot->png [s]
  (->> (parallel-mset s)
       (pixels->png s (format "output/mandelbrotp-%d.png" s))))

(defn -main [& args]
  (let [s (if (first args)
            (Integer/parseInt (first args))
            100)]
    (if (parallel-mandelbrot->png s)
      (prn (format "Generated output/mandelbrotp-%d.png" s))
      (prn "Failed to generated mandelbrot PNG!"))))

(comment

  (time (mandelbrot->png 1000))
  (time (parallel-mandelbrot->png 8000))

  ;;
  )
