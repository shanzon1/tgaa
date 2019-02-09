(ns tgaa.algo.core
  (:import [java.awt.image BufferedImage]
           [tgaa.hull ConvexHull Point])
  (:require [tgaa.algo.phases :as phase]
            [tgaa.struct.shared :as shared]
            [tgaa.util.visualize :as  viz]))

(defn process-image [] 
 (let[_ (shared/init-trail-state) 
     _ (phase/load-image) 
     _ (shared/time-start)
     _ (phase/bootstrap) 
     _ (phase/trapping) 
     _ (phase/evaluation)
     _ (shared/time-end)
     ]
     (viz/animate-algo)))
