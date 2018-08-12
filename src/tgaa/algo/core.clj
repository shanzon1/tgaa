(ns tgaa.algo.core
  (:import [java.awt.image BufferedImage])
  (:require [tgaa.algo.phases :as phase]))




(defn process-image [] 
  (do(tgaa.util.shared/init-trail-state)
    (phase/load-image)
       (phase/bootstrap)
       (phase/trapping)
       (phase/evaluation)))
        
;(tgaa.util.image/anim-trail-paths)