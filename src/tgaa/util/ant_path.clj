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
    {:start start-point :end nil :dir dir-opt :thresh false :trial-num (shared/trial-num)}))


(defn num-of-phero-starts []
  "Creates number of pheromone starts based on config and session values"
  (int 
    (* 
      (:trial-num @shared/session) 
      (:plac-heur shared/config) 
      (:num-ants shared/config))))
 
(defn num-of-random-starts []
  "Creates number of random starts based on config and session values"
     (int
       (- (:num-ants shared/config)
       (* 
         (:trial-num @shared/session) 
         (:plac-heur shared/config)
         (:num-ants shared/config)))))

(defn phero-points [num]
               (do (print "not implemented")
                 (repeat num [100 100])))


(defn init-trail-paths [^BufferedImage image]
  "Gets ant paths for a trail based on session and config"
  (map #(ant-path % image)
       (concat 
         (random-point 
           (num-of-random-starts) image)
         (phero-points 
           (num-of-phero-starts)))))

(defn compare-two-points [point-ref point-comp image compare-type]
  "returns point-comp if point-ref is null; point-compare returned on if true given compare-type :less :great"
  (if (nil? point-ref)
    point-comp
    (let [ref-val (. image getRGB (first point-ref)(second point-ref))
          comp-val (. image getRGB (first point-comp)(second point-comp))] 
      (if (= :less compare-type)
        (if (> ref-val comp-val)
          point-comp
          point-ref)
         (if (< ref-val comp-val)
          point-comp
          point-ref)))))     
     

(defn proc-ant [ant-path image]
  "Takes ants and image and generates logical paths"
  (loop [i 0 thresh? false end-pont nil local-min nil local-max nil]
    (if (or (>= i (:max-path-length shared/config)) thresh?)
      (assoc ant-path :thresh thresh? :end end-pont :local-min local-min :local-max local-max)
      (let [[x y] (path-loc-at-time ant-path i)]
        (recur (inc i) 
               (if (> (:thresh @shared/session) (. image getRGB x y))
                 true false)
               [x y]
               (compare-two-points local-min [x y] image :less)
               (compare-two-points local-max [x y] image :great))))))

(defn proc-all-ants [ant-init image]
  (map #(proc-ant % image) ant-init))

(defn trial-path-point-vals[ant-paths att-key image]
  "gets point value of attribute att-key  of ant paths"
  (map (fn [ant-path] 
         (let [{ local-max att-key} ant-path
               [x y] local-max] (. image getRGB x y ))) ant-paths))

(defn trial-min-local[ant-paths  image]
  "gets min value of all paths"
  (trial-path-point-vals ant-paths :local-min image))

(defn trial-max-local[ant-paths  image]
  "gets max values of all paths"
  (trial-path-point-vals ant-paths :local-max image))

(defn trail-min-of-max[ant-paths image]
  (apply min (trial-max-local ant-paths image)))

(defn trail-max-of-min[ant-paths image]
  (apply max (trial-min-local ant-paths image)))

(defn escaped-ants [ant-paths]
  (filter #(= (:thresh %) false) ant-paths))
