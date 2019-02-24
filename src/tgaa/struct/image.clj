(ns tgaa.struct.image
  (:require [tgaa.struct.shared :as shared]
            [clojure.test :refer [is]]
            [tgaa.util.filters :as filter]
            [clojure.string :refer [upper-case]]
            [tgaa.struct.ant :as ant]
            [mikera.image.core :as mi])
  (:import  (javax.imageio ImageIO)
            (java.io File)
            (java.awt.image BufferedImage)
            (java.awt Color BasicStroke Polygon)))

(defn load-import[]
  (do
    (clojure.core/import*
      "java.awt.Color")))
  
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

(defn pix-value-upd 
  ([x y val image]
  {:pre [(and (is (< x (. image getWidth))) 
              (is (<= 0 x)) 
              (is(< y (. image getHeight))) 
              (is (<= 0 y)))]}
  (. image setRGB x y val))
  ([[x y] val image]
    (pix-value-upd x y val image)))

(defn image-height [image]
  (. image getHeight))

(defn image-width [image]
  (. image getWidth))

(defn build-graphic [line-width color-name-str img-ref]
  (doto (. img-ref createGraphics)
    (.setColor (eval (read-string 
                       (str "(. Color "  
                            (upper-case color-name-str) ")"))))
    (.setStroke (BasicStroke. line-width))))

(defn draw-string [string graphic x y]
  (. graphic drawString string x y))
