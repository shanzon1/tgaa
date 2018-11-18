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

(defn pix-value 
  ([x y image]
  {:pre [(and (is (< x (. image getWidth))) 
              (is (<= 0 x)) 
              (is(< y (. image getHeight))) 
              (is (<= 0 y)))]}
  (shared/target-intensity (Math/abs (bit-shift-right (. image getRGB x y) 16))))
  ([[x y] image]
    (pix-value x y image)))

(defn image-height [image]
  (. image getHeight))

(defn image-width [image]
  (. image getWidth))

(defn show-image []
  (mi/show (shared/image-ref)))

(defn draw-ant-path [{:keys [start end]} graphic]
  (. graphic drawLine (first start) (second start) (first end) (second end)))

(defn draw-ant-point [point graphic]
  (. graphic drawOval (first point) (second point) 2 2))

(defn build-graphic [line-width color-name-str img-ref]
  (doto (. img-ref createGraphics)
    (.setColor (eval (read-string 
                       (str "(. Color "  
                            (upper-case color-name-str) ")"))))
    (.setStroke (BasicStroke. line-width))))

(defn draw-ant-end-pnts [ant-paths img-ref]
  (let [i (mi/copy img-ref)
        g (build-graphic 1 "YELLOW" i)
        _  (doall (map #(draw-ant-point (ant/ant-end-point %) g) ant-paths))]   
    i))

(defn draw-ant-start-pnts [ant-paths img-ref]
  (let [i (mi/copy img-ref)
        g (build-graphic 1 "BLUE" i)
        _  (doall (map #(draw-ant-point (ant/ant-start-point %) g) ant-paths))]
  i))

(defn draw-paths [ant-paths color-name-str img-ref line-width]
  (let [g (build-graphic line-width color-name-str img-ref)]
    (doall (map 
             #(draw-ant-path % g) ant-paths))
    img-ref))

(defn filtered-draw-paths [ant-paths-filter-func color-name-str img-ref line-width]
    (draw-paths (filter ant-paths-filter-func (shared/canidates)) color-name-str img-ref line-width))

(defn draw-eval-paths []
  (let [i (mi/copy (shared/image-ref))]
    (draw-paths (shared/eval-paths) "YELLOW" i 2)))

(defn show-val-end-pnts []
  (mi/show (draw-ant-end-pnts (shared/eval-paths) (shared/image-gry-ref))))

(defn show-val-start-pnts []
  (mi/show (draw-ant-start-pnts (shared/eval-paths) (shared/image-gry-ref))))

(defn show-val-boundry-pnts []
  (mi/show (draw-ant-start-pnts
             (shared/eval-paths) 
             (draw-ant-end-pnts (shared/eval-paths) (shared/image-gry-ref)))))

(defn show-eval-paths[]
  (mi/show (draw-eval-paths)))

(defn draw-can-paths
  ([]
    (let [i (mi/copy (shared/image-ref))]
    (filtered-draw-paths (fn [x] true) "YELLOW" i 2)
      i))
    ([trial-num]
      (let [i (mi/copy (shared/image-ref))]
    (filtered-draw-paths (fn [x] (= (ant/ant-trial-num x) trial-num)) "YELLOW" i 2)
      i)))

(defn show-segmentaton 
   ([theshold]
    (mi/show ((filter/threshold theshold) (shared/image-ref))))
  ([]
  (show-segmentaton (shared/thresh))))


(defn show-cann-path
  ([]
    "show all trap paths"
    (mi/show (draw-can-paths)))
  ([trial-num] 
    "show trap path for i th trial"
    (mi/show (draw-can-paths trial-num))))

(defn anim-trail-paths[]
  (loop [i 0]
    (if (= i (shared/get-num-trails))
      nil
      (do (Thread/sleep 1000) (show-cann-path i) (recur (inc i))))))
  
(defn draw-boundary [ref-img boundary]
  (let [boundary-closed (concat boundary [(first boundary)])
        xpnts (doall (map first  boundary-closed))
        ypnts (doall (map second boundary-closed))
        p-num (count boundary-closed)
        g (. ref-img createGraphics)
        _ (doto g (.setColor (. Color YELLOW))
            (.setStroke (java.awt.BasicStroke. 4))
            (.drawPolyline (int-array p-num xpnts) (int-array p-num ypnts) p-num))]
    ref-img))

(defn draw-final-boundary[]
  (mikera.image.core/show
    (draw-boundary (mikera.image.core/copy (tgaa.struct.shared/image-ref))
                   (tgaa.struct.shared/hull))))

(defn draw-ant-paths[]
  (mikera.image.core/show
    (draw-boundary (mikera.image.core/copy (tgaa.struct.shared/image-ref))
                   (tgaa.struct.shared/hull))))

(defn animate-algo []
  (do (anim-trail-paths)
    (Thread/sleep 1000) 
    (draw-final-boundary)))