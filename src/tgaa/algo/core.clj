(ns tgaa.algo.core
  (:import [java.awt.image BufferedImage]
           [tgaa.hull ConvexHull Point])
  (:require [tgaa.algo.phases :as phase]
            [tgaa.struct.shared :as shared]))

(defn process-image [] 
 (map doall [(shared/init-trail-state) (phase/load-image) (phase/bootstrap) (phase/trapping) (phase/evaluation)]))

        
;(tgaa.util.image/anim-trail-paths)