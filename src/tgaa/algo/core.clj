(ns tgaa.algo.core
  (:import [java.awt.image BufferedImage])
  (:require [tgaa.util.image :as image]
            [tgaa.util.ant-path :as ap]
            [tgaa.util.shared :as shared]))

(defn bootstrap[]
  (apply min (ap/trial-min-local 
               (ap/proc-all-ants (ap/init-trail-paths)))))

(defn perform-trial []
  (let [trial-paths (ap/proc-all-ants (ap/init-trail-paths))
        _  (shared/add-canidates (ap/trapped-ants trial-paths))
        _  (shared/update-thresh  (ap/trap-escaped-thresh trial-paths))]))

(defn process-image [] 
  (let [_ (shared/save-image-ref (image/image-RGB-gray (image/get-image))) ;aquire image
        _ (shared/update-thresh (bootstrap)) ;bootstrap
        _ (repeatedly (shared/get-num-trails) #(perform-trial)) ;trapping
         ]
    (shared/thresh)))
        