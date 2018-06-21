(ns tgaa.util.image
  (:require [tgaa.util.shared :refer [session config]]
            [tgaa.util.image :refer :all])
  (:import [javax.imageio ImageIO]
            [java.io File]
            [java.awt.image BufferedImage]))

(defn get-image []
  "Takes a map with :imageLocation and returns assocated BufferedImage"
  (try
    (. ImageIO read (File.  
       (:imageLocation @session)))
    (catch Exception e 
      (do (println "Image not found. Check location is correct")
        (throw (Exception.  e))))))

(defn image-RGB-gray [^BufferedImage image]
   (let [ out-image (BufferedImage. (. image getWidth) (. image getHeight) (. BufferedImage TYPE_BYTE_GRAY))
          _ (. (. out-image getGraphics) drawImage image 0 0 nil)]
     out-image))


