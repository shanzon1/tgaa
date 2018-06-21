(ns tgaa.util.trailUtil
  (:require [tgaa.util.shared :refer :all])
  (:import [java.awt.image BufferedImage]))

(def dir-sel [[0 1][0 2][1 0][2 0][1 1][2 2][1 2][2 1]])

(defn path-last-point-gen [dir]
  "Get last points of gen axis of a path for performance"
  (fn [start]
    (cond 
      (= 0 dir)
      start
      (= 1 dir)
      (+ start (- (:maxPathLength config) 1))
      :else
      (+ (- start  (:maxPathLength config) ) 1))))

(defn path-generator [dir]
  "Generates list of functions for one axis of a path"
  (fn [start]
    (let [path-len (:maxPathLength config)]
      (cond 
        (= 0 dir)
        (repeat path-len start) 
        (= 1 dir)
        (range start (+ start path-len))
        (= 2 dir)
        (range (- (inc start) path-len) (inc start))
        :else nil))))
  
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

(defn ant-valid-dir 
  "Creates a random path 45 deg increments with starting point x y and length"
  [x y image] 
   (:dir-sel (first 
               (filter #(let [lx (first (:last %))
                              ly (second (:last %))]
                          (and (> lx 0) (> ly 0)) (< lx (. image getWidth)) (< ly (. image getHeight)))
                       (map (fn [d] {:last [((path-last-point-gen (first d)) x) ((path-last-point-gen (second d)) y)] 
                                          :dir-sel d})  (shuffle dir-sel))))))

(defn ant-path [x y image]
  "Creates an ant path. Path must be valid or exception is thrown"
  (let [dir-sel (ant-valid-dir x y image)]
    (map 
      #(vector %1 %2 (. image getRGB %1 %2))
    ((path-generator (first dir-sel)) x ) 
    ((path-generator (second dir-sel)) y))))

(defn get-trail-paths [image]
  "Gets ant paths for a trail based on session and config"
     (let [numPlacedBiased (int 
                             (* 
                               (:trialNum @session) 
                               (:placementHeuristic config) 
                               (:numAnts config))) 
           randPaths (map #(ant-path (nth % 0) 
                                     (nth % 1)
                                      image) 
                          (random-staring-coords image))
           ;biasPaths (if ((:canPaths session)
           ;need a good data struct thinking map  {:x-y [x y] 
           ]
     randPaths))