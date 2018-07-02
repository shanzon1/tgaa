(ns tgaa.algo.core
  (:import [java.awt.image BufferedImage])
  (:require [tgaa.util.image :as image]
            [tgaa.util.ant-path :as ap]
            [tgaa.util.shared :as shared]))

(defn process-image [] 
  (let [image ^BufferedImage (image/image-RGB-gray (image/get-image))
        _  (shared/update-thresh
             (apply max
                    (ap/random-starting-coords (:num-ants config) image)))]
             ))
        