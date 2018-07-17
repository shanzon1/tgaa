(ns tgaa.util.image
  (:require [tgaa.util.shared :as shared]
            [clojure.test :refer [is]]
            [tgaa.java.util.filters :as filter]
            [clojure.string :refer [upper-case]])
  (:import [javax.imageio ImageIO]
            [java.io File]
            [java.awt.image BufferedImage]
            [java.awt BasicStroke Color]))

(use 'mikera.image.core)


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
  (show (shared/image-ref)))

(defn show-segmentaton []
  (show ((filter/threshold (shared/thresh)) (shared/image-ref))))

(defn draw-paths [ant-paths-filter-func color-name-str img-ref line-width]
  (let [g (doto (. img-ref createGraphics)
            (.setColor (eval (read-string 
                               (str "(. Color "  
                                    (upper-case color-name-str) ")"))))
            (.setStroke (BasicStroke. line-width)))]
    (doall (map 
             (fn [{:keys [start end]}] 
               (. g drawLine (first start) (second start) (first end) (second end))) 
             (filter ant-paths-filter-func (tgaa.util.shared/canidates))))))

(defn draw-all-can-paths[]
  (draw-paths (fn [x] true) "YELLOW" (tgaa.util.shared/image-ref) 2))
