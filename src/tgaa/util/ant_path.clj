(ns tgaa.util.ant-path
  (:require [tgaa.util.shared :as shared])
  (:import [java.awt.image BufferedImage]))

(def dir-opt [[0 1][0 -1][1 0][-1 0][1 1][-1 -1][1 -1][-1 1]])

(defn path-loc-at-time [ant-path time]
  [(+ (first (:start ant-path)) (* (first (:dir ant-path)) time))
  (+ (second (:start ant-path)) (* (second (:dir ant-path)) time))])
  
(defn full-path-last-point [start dir]
  "Get last points of gen axis of a path for performance"
    (cond 
      (= 0 dir)
      start
      (= 1 dir)
      (+ start (- (:max-path-length shared/config) 1))
      :else
      (+ (- start  (:max-path-length shared/config) ) 1)))
  
(defn rand-ant-dir 
  "Creates safe random direction at 45 deg increments with starting point x y"
  [point] 
   (:dir-opt (first 
               (filter #(let [lx (first (:last %))
                              ly (second (:last %))]
                          (and (> lx 0) (> ly 0)) (< lx (. (shared/image-ref) getWidth)) (< ly (. (shared/image-ref) getHeight)))
                       (map (fn [d] {:last [(full-path-last-point (first point)(first d)) 
                                            (full-path-last-point (second point) (second d))] 
                                          :dir-opt d})  (shuffle dir-opt))))))

(defn random-point 
  "Get random set of coordinates"
  [num-loc]
  (partition 2
             (interleave 
               (repeatedly 
                      num-loc
                      #(rand-int 
                         (. (shared/image-ref) getWidth)))
        (repeatedly 
               num-loc 
               #(rand-int 
                  (. (shared/image-ref) getHeight))))))

(defn ant-path [start-point]
  "Creates a logical ant path"
  (let [dir-opt (rand-ant-dir start-point)]
    {:start start-point :end nil :dir dir-opt :thresh false :trial-num (shared/trial-num)}))


(defn num-of-phero-starts []
  "Creates number of pheromone starts based on config and trail-state values"
  (int 
    (* 
      (:trial-num @shared/trail-state) 
      (:plac-heur shared/config) 
      (:num-ants shared/config))))
 
(defn num-of-random-starts []
  "Creates number of random starts based on config and trail-state values"
     (int
       (- (:num-ants shared/config)
       (* 
         (:trial-num @shared/trail-state) 
         (:plac-heur shared/config)
         (:num-ants shared/config)))))

(defn phero-points [num]
               (do (print "not implemented")
                 (repeat num [100 100])))


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
    (let [ref-val (. (shared/image-ref) getRGB (first point-ref)(second point-ref))
          comp-val (. (shared/image-ref) getRGB (first point-comp)(second point-comp))] 
      (if (= :less compare-type)
        (if (> ref-val comp-val)
          point-comp
          point-ref)
         (if (< ref-val comp-val)
          point-comp
          point-ref)))))     
     

(defn proc-ant [ant-path]
  "Takes ants and image and generates logical paths"
  (loop [i 0 thresh? false end-pont nil local-min nil local-max nil]
    (if (or (>= i (:max-path-length shared/config)) thresh?)
      ; needs to be extracted to shared only
      (assoc ant-path :thresh thresh? :end end-pont :local-min local-min :local-max local-max)
      (let [[x y] (path-loc-at-time ant-path i)]
        (recur (inc i) 
               (if (> (:thresh @shared/trail-state) (. (shared/image-ref) getRGB x y))
                 true false)
               [x y]
               (compare-two-points local-min [x y]  :less)
               (compare-two-points local-max [x y]  :great))))))

(defn proc-all-ants [ant-init]
  (if-not (empty? ant-init)
  (map #(proc-ant %) ant-init)))

;
(defn trial-path-point-vals[ant-paths att-key]
  "gets point value of attribute att-key  of ant paths"
  (if-not (empty? ant-paths)
    (map (fn [ant-path] 
           (let [{ local-max att-key} ant-path
                 [x y] local-max] (. (shared/image-ref) getRGB x y ))) ant-paths)))

(defn trial-min-local[ant-paths]
  "gets min value of all paths"
  (if-not (empty? ant-paths)
    (trial-path-point-vals ant-paths :local-min)))

(defn trial-max-local[ant-paths]
  "gets max values of all paths"
  (if-not (empty? ant-paths)
    (trial-path-point-vals ant-paths :local-max)))

(defn trail-min-of-max[ant-paths]
  (if-not (empty? ant-paths)
    (apply min (trial-max-local ant-paths))))

(defn trail-max-of-min[ant-paths]
  (if-not (empty? ant-paths)
    (apply max (trial-min-local ant-paths))))

(defn escaped-ants [ant-paths]
  (if-not (empty? ant-paths)
    (filter #(= (:thresh %) false) ant-paths)))

(defn trapped-ants [ant-paths]
  (if-not (empty? ant-paths)
    (filter #(= (:thresh %) true) ant-paths)))

(defn trap-escaped-thresh [ant-paths]
  (if-not (empty? ant-paths)
    (trail-max-of-min (escaped-ants ant-paths))))
