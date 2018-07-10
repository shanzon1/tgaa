(ns tgaa.util.image
  (:require [tgaa.util.shared :as shared]
            [clojure.test :refer [is]])
  (:import [javax.imageio ImageIO]
            [java.io File]
            [java.awt.image BufferedImage]))

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
  (. image getRGB x y))

(defn image-height [image]
  (. image getHeight))

(defn image-width [image]
  (. image getWidth))
