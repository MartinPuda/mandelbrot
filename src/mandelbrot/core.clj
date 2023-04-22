(ns mandelbrot.core
  (:require [complex.core :as c]
            [clojure.math :as m])
  (:import (javax.swing JFrame JPanel)
           (java.awt Dimension Graphics Color)
           (org.apache.commons.math3.complex Complex)
           (java.awt.image BufferedImage))
  (:gen-class))

(defn c-seq [^Complex c]
  (->> (iterate #(c/+ (c/pow % 2) c) c)
       (map (comp m/round c/abs))))

(defn converges? [sq]
  (every? #(>= 2 %) sq))

(defn pos->c [x y]
  (c/complex (/ (- x 500.0) 250)
             (/ (- 500.0 y) 250)))

(defn mandelbrot-window []
  (let [width 1000
        height 1000
        iter 25]
    (doto (JFrame. "Mandelbrot")
      (.setPreferredSize (Dimension. width height))
      (.add (proxy [JPanel] []
              (paintComponent [^Graphics g]
                (let [b (BufferedImage. width height BufferedImage/TYPE_INT_ARGB)]
                  (dotimes [y height]
                    (dotimes [x width]
                      (let [[i v]
                            (->> (c-seq (pos->c x y))
                                 repeat
                                 (map #(take %1 %2) (range iter))
                                 (map-indexed vector)
                                 (take-while (fn [[i v]] (converges? v)))
                                 last)
                            result
                            (if (= i 24)
                              (.getRGB (Color. 0 0 0))
                              (.getRGB (Color. ^long (* i (long (/ 255 iter)))
                                               ^long (* i (long (/ 255 iter)))
                                               ^long (* i (long (/ 255 iter)))
                                               ^long (- 255 (* i (long (/ 255 iter)))))))]
                        (.setRGB b x y result))))
                  (.drawImage g b 0 0 nil))))))))

; After calling (-main), wait for a few seconds.

(defn -main [& args]
  (doto (mandelbrot-window)
    .pack
    (.setVisible true)))