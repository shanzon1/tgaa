(ns tgaa.algo.core
  (:import [java.awt.image BufferedImage]
           [tgaa.hull ConvexHull Point])
  (:require [tgaa.algo.phases :as phase]
            [tgaa.struct.shared :as shared]
            [tgaa.util.visualize :as  viz]))

(defn process-image [] 
 (do (shared/init-trail-state) 
     (phase/load-image) 
     (shared/time-start)
     (phase/bootstrap) 
     (shared/update-thresh 1)
     (phase/trapping) 
     (phase/evaluation)
     (shared/time-end)
     ;(phase/analysis-hull)
     ))

        
;(tgaa.util.image/anim-trail-paths)