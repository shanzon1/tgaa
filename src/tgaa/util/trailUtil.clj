(ns tgaa.util.trailUtil
  (:require [tgaa.util.shared :refer :all])
  (:import [java.awt.image BufferedImage]))

(def dir-opt [[0 1][0 2][1 0][2 0][1 1][2 2][1 2][2 1]])

(defn full-path-last-point [start dir]
  "Get last points of gen axis of a path for performance"
    (cond 
      (= 0 dir)
      start
      (= 1 dir)
      (+ start (- (:max-path-length config) 1))
      :else
      (+ (- start  (:max-path-length config) ) 1)))
  
(defn rand-ant-dir 
  "Creates safe random direction at 45 deg increments with starting point x y"
  [point ^BufferedImage image] 
   (:dir-opt (first 
               (filter #(let [lx (first (:last %))
                              ly (second (:last %))]
                          (and (> lx 0) (> ly 0)) (< lx (. image getWidth)) (< ly (. image getHeight)))
                       (map (fn [d] {:last [(full-path-last-point (first point)(first d)) 
                                            (full-path-last-point (second point) (second d))] 
                                          :dir-opt d})  (shuffle dir-opt))))))

(defn random-point 
  "Get random set of coordinates"
  [num-loc ^BufferedImage image]
  (partition 2
             (interleave 
               (repeatedly 
                      num-loc
                      #(rand-int 
                         (. image getWidth)))
        (repeatedly 
               num-loc 
               #(rand-int 
                  (. image getHeight))))))

(defn ant-path [start-point ^BufferedImage image]
  "Creates a logical ant path"
  (let [dir-opt (rand-ant-dir start-point image)]
    {:start start-point :end nil :dir dir-opt :thresh false}))


(defn num-of-random-starts []
  "Creates number of random starts based on config and session values"
  (int 
    (* 
      (:trial-num @session) 
      (:plac-heur config) 
      (:num-ants config))))
 
(defn num-of-phero-starts[]
  "Creates number of pheromone starts based on config and session values"
     (int
       (- (:num-ants config)
       (* 
         (:trial-num @session) 
         (:plac-heur config)
         (:num-ants config)))))

(defn phero-points [num]
               (do (print "not implemented")
                 (repeat 10 [1 1])))

(defn get-trail-paths [^BufferedImage image]
  "Gets ant paths for a trail based on session and config"
  (map #(ant-path % image)
       (apply conj 
              (random-point 
                (num-of-random-starts) image)
              (phero-points 
                (num-of-phero-starts)))))

