(ns tgaa.util.trailUtil
  (:require [tgaa.util.shared :refer :all])
  (:import [java.awt.image BufferedImage]))

(defn random-staring-coords 
  "Get random set of coordinates"
  [^BufferedImage image]
  (partition 2
             (interleave 
               (repeatedly 
                      (:numAnts config) 
                      #(rand-int 
                         (. image getWidth)))
        (repeatedly 
               (:numAnts config) 
               #(rand-int 
                  (. image getHeight))))))


(defn get-path 
  "Creates a random path 45 deg increments with starting point x y and length"
  [x y length]  
  (let [path-gen-list (fn [length start] [(repeat length start) 
                                          (map #(+ %1 %2) 
                                               (range 0 length)
                                               (repeat length start))
                                          (map #(+ %1 %2) 
                                               (map #(* -1 %1)  (range 0 length))
                                               (repeat length start))])
        valid-comb [[0 1][0 2][1 0][2 0][1 1][2 2][1 2][2 1]]
        dir-sel (rand-nth valid-comb)
        ]
    (partition 2 (interleave (nth (path-gen-list length x) (first dir-sel)) (nth (path-gen-list length y) (second dir-sel))))))


(defn get-trail-paths [image]
  "Gets ant paths for a trail based on session and config"
     (let [numPlacedBiased (int 
                             (* 
                               (:trialNum @session) 
                               (:placementHeuristic config) 
                               (:numAnts config))) 
           randPaths (map #(get-path (nth % 0) 
                                     (nth % 1)
                                     (:maxPathLength config)) 
                          (random-staring-coords 
                            (tgaa.util.image/get-image)))]
     randPaths))