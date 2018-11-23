(ns tgaa.algo.ant-path
  (:require [tgaa.struct.shared :as shared]
            [tgaa.struct.image :as image]
            [tgaa.algo.trial :as trial] 
            [tgaa.struct.ant :as ant])
  (:import [java.awt.image BufferedImage]))

(defn full-path-last-point [start dir]
  "Get last points of gen axis of a path for performance"
    (cond 
      (= 0 dir)
      start
      (= 1 dir)
      (+ start (dec (shared/max-path-length)))
      :else
      (- start (dec (shared/max-path-length)))))

(defn rand-ant-dir 
  "Creates safe random direction at 45 deg increments with starting point x y"
  [point]
  (rand-nth ant/dir-opt))

(defn random-point 
  "Get random set of coordinates"
  [num-loc]
  (partition 2
             (interleave 
               (repeatedly 
                      num-loc
                      #(rand-int 
                         (dec (image/image-width (shared/image-gry-ref)))))
        (repeatedly 
               num-loc 
               #(rand-int 
                  (dec (image/image-height (shared/image-gry-ref))))))))

(defn ant-path [start-point]
  "Creates an ant path"
  (->> (ant/create-ant-path) 
    (ant/ant-trial-num (shared/trial-num))
    (ant/ant-thresh? false) 
    (ant/ant-dir-vec (rand-ant-dir start-point))
    (ant/ant-start-point start-point)))


(defn num-of-phero-starts []
  "Creates number of pheromone starts based on config and trail-state values"
  (int 
    (* 
      (shared/trial-num) 
      (:plac-heur shared/config) ;this needs to be more flexible
      (shared/num-ants))))
 
(defn num-of-random-starts []
  "Creates number of random starts based on config and trail-state values"
     (int
       (- (shared/num-ants)
       (* 
         (shared/trial-num) 
         (:plac-heur shared/config);this needs to be more flexible
         (shared/num-ants)))))



(defn phero-points [num]
  "radomized ants gets one random point in each <num> paths from cand set"
  (if (and (not (empty? (shared/canidates))) (< 0 num))
    (let [bias-paths (take num (cycle (shuffle (shared/canidates))))
          lengths (map #(ant/ant-path-length %) bias-paths)
          start-loc (map #(ant/path-loc-at-time %1 %2)
                         bias-paths
                         (map rand-int lengths))]
      
    start-loc)
    (vector)))
  

(defn init-trail-paths []
  "Gets ant paths for a trail based on trail-state and config"
  (map #(ant-path %)
       (concat 
         (random-point 
           (num-of-random-starts))
         (phero-points 
           (num-of-phero-starts)))))

(defn compare-two-points [point-ref point-comp compare-type]
  "returns point-comp if point-ref is null; point-compare returned on if true given compare-type :less :great"
  (if (nil? point-ref)
    point-comp
    (let [ref-val (image/pix-value (first point-ref)(second point-ref) (shared/image-gry-ref))
          comp-val (image/pix-value (first point-comp)(second point-comp) (shared/image-gry-ref))] 
      (if (= :less compare-type)
        (if (> ref-val comp-val)
          point-comp
          point-ref)
         (if (< ref-val comp-val)
          point-comp
          point-ref)))))     

(defn create-ant-path [length ant-status end-point  local-min  local-max ant-path] 
  (if  (= ant-status ant/status-dead)
    (->> ant-path 
        (ant/ant-thresh? false)
        (ant/ant-status ant-status))
    (->> ant-path 
      (ant/ant-thresh? (= ant-status ant/status-trapped))
      (ant/ant-end-point (ant/path-loc-at-time ant-path (- length (shared/min-cont-thresh))))
      (ant/ant-local-min local-min)
      (ant/ant-local-max local-max)
                 (ant/ant-status ant-status)
      (ant/ant-path-length (- length (shared/min-cont-thresh))))))

(defn thresh-counter [x y thresh-count]
  (if (and (not= (shared/thresh) shared/no-threshold)
           (> (image/pix-value  x y (shared/image-gry-ref)) (shared/thresh)))
    (inc thresh-count)
    0))

(defn path-status [length thresh-count ant-path]
  (let [[x y] (ant/path-loc-at-time ant-path length)
        iy (dec (image/image-height (shared/image-gry-ref)))
        ix (dec (image/image-width (shared/image-gry-ref)))]
    (cond 
      (>= length  (shared/max-path-length)) 
      ant/status-escaped
      (and (>= thresh-count (shared/min-cont-thresh)) (> length (shared/min-path-len)))
      ant/status-trapped
      (or
        (and (>= thresh-count (shared/min-cont-thresh)) (< length (shared/min-path-len)))
        (> x ix)
        (> y iy)
        (< x 1)
        (< y 1))
      ant/status-dead
      :else
      ant/status-forage)))

(defn proc-ant [ant-path]
  "Takes ants and image and generates logical paths"
  (loop [i 0 thresh-count 0 local-min nil local-max nil [x y] (ant/ant-start-point ant-path)]
    (let [ant-status (path-status i thresh-count ant-path)]
      (if (not= ant-status ant/status-forage)     
        (create-ant-path i ant-status [x y] local-min  local-max ant-path)
        (recur (inc i) 
               (thresh-counter x y thresh-count)
               (compare-two-points local-min [x y]  :less)
               (compare-two-points local-max [x y]  :great)
               (ant/path-loc-at-time ant-path i))))))

(defn proc-all-ants [ant-init]
  (if-not (empty? ant-init)
    (let [paths (map #(proc-ant %) ant-init)]
      paths)))


