(ns tgaa.algo.core
  (:import [java.awt.image BufferedImage])
  (:require [tgaa.util.image :refer :all]
            [tgaa.algo.bootstrap :refer :all]
            [tgaa.util.shared :refer [session config]]))

(defn process-image [] 
  (let [image ^BufferedImage (image-RGB-gray (get-image))
        workingImage ^BufferedImage (BufferedImage. (. image getWidth) (. image getHeight) (. image getType) )
        bootstrapThresh (bootstrap-value image)]
    bootstrapThresh))
