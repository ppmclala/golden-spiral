(ns mandelbrot
  (:require
   [clojure.string :as str])
  (:import
   [java.awt.image BufferedImage]
   [java.io File]
   [javax.imageio ImageIO]))

(def max-iterations 1000)

(defn rgba->argb-pixel [r g b a]
  (unchecked-int
   (bit-or (bit-shift-left (bit-and a 0xFF) 24)
           (bit-or (bit-shift-left (bit-and r 0xFF) 16)
                   (bit-or (bit-shift-left (bit-and g 0xFF) 8)
                           (bit-and b 0xFF))))))

(def MSET    (rgba->argb-pixel   0   0   0 255))
(def NEAR    (rgba->argb-pixel 190   0 255 255))
(def NEARER  (rgba->argb-pixel 255 255   0 255))
(def NEAREST (rgba->argb-pixel   0 255   0 255))
(def FAR     (rgba->argb-pixel  24  24  24 255))

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
     (and (< i max-iterations)
          (<= (+ (* x x) (* y y)) (* 2 2)))
      (recur (+ (- (* x x) (* y y)) cx)
             (+ (* 2 x y) cy)
             (inc i))
      i)))

(defn px->cx [x sz] (scale x sz -2.0 0.5))
(defn py->cy [y sz] (scale y sz -1.25 1.25))

(defn idx->pixel [i sz]
  (let [cx (px->cx (rem i sz) sz)
        cy (py->cy (quot i sz) sz)]
    (-> (calc-escapes cx cy) ->color)))

(defn process-tile! [array sz tile-idxs]
  (doseq [idx tile-idxs]
    (aset-int array idx (idx->pixel idx sz))))

(defn tiled-mset->pixels [n]
  (let [sz (* n n)
        pxs (int-array sz)]
    (doall
     (->> (range sz)
          (partition-all 10000)
          (pmap #(process-tile! pxs n %))))
    pxs))

(defn pixels->png [sz png-path pixel-array]
  (let [canvas (BufferedImage. sz sz BufferedImage/TYPE_INT_ARGB)]
    (.setRGB canvas 0 0 sz sz pixel-array 0 sz)
    (ImageIO/write canvas "png" (File. png-path))))

(defn mset->png [s]
  (->> (tiled-mset->pixels s)
       (pixels->png s (format "output/mandelbrotp-%d.png" s))))

(defn mset->frequencies [sz]
  (->>
   (for [x* (range sz)
         y* (range sz)]
     (calc-escapes (px->cx x* sz) (py->cy y* sz)))
   frequencies
   (map #(str (first %) "," (last %)))
   (str/join "\n")
   (spit (str "output/frequencies-" sz ".csv"))))

(defn -main [& args]
  (let [s (if (first args)
            (Integer/parseInt (first args))
            100)]
    (if (mset->png s)
      (prn (format "Generated output/mandelbrot-%d.png" s))
      (prn "Failed to generated mandelbrot PNG!"))))

(comment

  (time (mset->frequencies 100))
  (time (mset->png 420))

  ;;
  )
