(ns tgaa.algo.core
  (:import [java.awt.image BufferedImage]
           [tgaa.hull ConvexHull Point])
  (:require [tgaa.algo.phases :as phase]
            [tgaa.struct.shared :as shared]
            [tgaa.util.visualize :as  viz]
            [tgaa.util.gui :as gui]
            [tgaa.util.dice-metric-analysis :as dm]))

;resolve repl dynamic import issue
(tgaa.struct.image/load-import)

(defn process-image [] 
  (do 
    (shared/time-start)
    (dorun (phase/bootstrap)) 
    (dorun (phase/trapping))
    (dorun (phase/evaluation))
    (shared/time-end)))

(defn process-setup []
  (do (dorun (shared/init-trail-state))
        (shared/image-loc 
          (str (gui/sys-view-ref "Select Image to process" :file)))
        (dorun (phase/load-image))))

(defn run-animation [] 
    (if (= (gui/option-dialog "Run animation?") 0 )
    (viz/animate-algo)))

(defn show-results[]
  (if (= (gui/option-dialog "Show Results?") 0)
    (viz/show-prediction-pnts)))


(defn run-as-app []
  (loop [quit? 0]
    (if (not= quit? 0)
      nil
      (recur (do 
               (process-setup)
               (process-image)
               (run-animation)
               (viz/show-eval-paths-result)
               (gui/option-dialog "Process another image?"))))))


;(run-as-app)
;(tgaa.util.dice-metric-analysis/dice-metric)
;(tgaa.util.image-band-filter/convert-yellow-image)