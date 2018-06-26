(ns tgaa.algo.bootstrap
  (:require [tgaa.util.image :refer :all]
            [tgaa.util.trailUtil :refer :all]
            [tgaa.util.shared :refer [session config]]))

(defn bootstrap-value [image] 
  (apply max
         (random-starting-coords (:numAnts config) image)))
