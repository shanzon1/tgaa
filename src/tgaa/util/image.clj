(ns tgaa.util.image
  (:require [tgaa.struct.shared :as shared]
            [clojure.test :refer [is]]
            [tgaa.util.filters :as filter]
            [clojure.string :refer [upper-case]]
            [tgaa.struct.ant :as ant]
            [mikera.image.core :as mi])
  (:import [javax.imageio ImageIO]
            [java.io File]
            [java.awt.image BufferedImage]
            [java.awt BasicStroke]
            [java.awt Color]))
(import 'java.awt.Color)
(import 'java.awt.Polygon)

(defn get-image [& abs-path]
  "Takes a map with :imageLocation and returns assocated BufferedImage"
  (let [ path (if (empty? abs-path) 
                (shared/image-loc) 
                (first abs-path))]
  (try
    (. ImageIO read (File. path))
    (catch Exception e 
      (do (println "Image not found. Check location is correct")
        (throw (Exception.  e)))))))

(defn image-RGB-gray [^BufferedImage image]
   (let [ out-image (BufferedImage. (. image getWidth) (. image getHeight) (. BufferedImage TYPE_BYTE_GRAY))
          _ (. (. out-image getGraphics) drawImage image 0 0 nil)]
     out-image))

(defn pix-value[x y image]
  {:pre [(and (is (< x (. image getWidth))) 
              (is (<= 0 x)) 
              (is(< y (. image getHeight))) 
              (is (<= 0 y)))]}
  (Math/abs (bit-shift-right (. image getRGB x y) 16)))

(defn image-height [image]
  (. image getHeight))

(defn image-width [image]
  (. image getWidth))

(defn show-image []
  (mi/show (shared/image-ref)))

(defn draw-paths [ant-paths-filter-func color-name-str img-ref line-width]
  (let [g (doto (. img-ref createGraphics)
            (.setColor (eval (read-string 
                               (str "(. Color "  
                                    (upper-case color-name-str) ")"))))
            (.setStroke (BasicStroke. line-width)))]
    (doall (map 
             (fn [{:keys [start end]}] 
               (. g drawLine (first start) (second start) (first end) (second end))) 
             (filter ant-paths-filter-func (shared/canidates))))))

(defn draw-can-paths
  ([]
    (let [i (mi/copy (shared/image-ref))]
    (draw-paths (fn [x] true) "YELLOW" i 2)
      i))
    ([trial-num]
      (let [i (mi/copy (shared/image-ref))]
    (draw-paths (fn [x] (= (ant/ant-trial-num x) trial-num)) "YELLOW" i 2)
      i)))

(defn show-segmentaton []
  (mi/show ((filter/threshold (shared/thresh)) (shared/image-ref))))

(defn show-cann-path
  ([] (mi/show (draw-can-paths)))
  ([trial-num] (mi/show (draw-can-paths trial-num))))

(defn anim-trail-paths[]
  (loop [i 0]
    (if (= i (shared/get-num-trails))
      nil
      (do (Thread/sleep 1000) (show-cann-path i) (recur (inc i))))))
  

(defn draw-boundary [ref-img boundary]
  (let [boundary-closed (concat boundary [(first boundary)])
        xpnts (map #(int (. % getX)) boundary-closed)
        ypnts (map  #(int (. % getY)) boundary-closed)
        p-num (count boundary-closed)
        g (. ref-img createGraphics)
        _ (doto g (.setColor (. Color BLUE))
            (.setStroke (java.awt.BasicStroke. 1))
            (.drawPolyline (int-array p-num xpnts) (int-array p-num ypnts) p-num))]
    (mikera.image.core/show ref-img)))

;(defn draw-final-boundary[]
;  (draw-boundary (mikera.image.core/copy (tgaa.struct.shared/image-ref)))
  
;(draw-boundary i (tgaa.algo.analysis/final-boundary))