(ns tgaa.util.ant-path
  (:require [tgaa.util.shared :as shared]
            [tgaa.util.image :as image])
  (:import [java.awt.image BufferedImage]))

(def dir-opt [[0 1][0 -1][1 0][-1 0][1 1][-1 -1][1 -1][-1 1]])

(defn trial-path-point-vals[ant-paths att-key]
  "gets point value of attribute att-key  of ant paths"
  (if-not (empty? ant-paths)
    (map (fn [ant-path] 
           (let [{ local-max att-key} ant-path
                 [x y] local-max] (image/pix-value x y (shared/image-ref)))) ant-paths)))

(defn path-loc-at-time [ant-path time]
  [(+ (first (:start ant-path)) (* (first (:dir ant-path)) time))
  (+ (second (:start ant-path)) (* (second (:dir ant-path)) time))])
  
(defn full-path-last-point [start dir]
  "Get last points of gen axis of a path for performance"
    (cond 
      (= 0 dir)
      start
      (= 1 dir)
      (+ start (- (shared/max-path-length) 1))
      :else
      (+ (- start  (shared/max-path-length)) 1)))
  
(defn rand-ant-dir 
  "Creates safe random direction at 45 deg increments with starting point x y"
  [point] 
   (:dir-opt (first 
               (filter #(let [lx (first (:last %))
                              ly (second (:last %))]
                          (and (> lx 0) (> ly 0)) (< lx (image/image-width (shared/image-ref))) (< ly (image/image-height (shared/image-ref))))
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
                         (image/image-width (shared/image-ref))))
        (repeatedly 
               num-loc 
               #(rand-int 
                  (image/image-height (shared/image-ref)))))))

(defn ant-path [start-point]
  "Creates a logical ant path"
  (let [dir-opt (rand-ant-dir start-point)]
    {:start start-point :end nil :dir dir-opt :thresh false :trial-num (shared/trial-num)}))


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


;;;;;;;;;;;;; not tested 
(defn phero-points [num]
  (if-not (empty? (shared/canidates))
    (let [num (if (> num (count (shared/canidates)))
                     (count (shared/canidates))
                     num)]
      (path-loc-at-time (take num (shuffle (shared/canidates))) (rand-int shared/max-path-length))))
  (vector))
    

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
    (let [ref-val (image/pix-value (first point-ref)(second point-ref) (shared/image-ref))
          comp-val (image/pix-value (first point-comp)(second point-comp) (shared/image-ref))] 
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
    (if (or (>= i  (shared/num-ants)) thresh?)
      ; needs to be extracted to shared only
      (assoc ant-path :thresh thresh? :end end-pont :local-min local-min :local-max local-max)
      (let [[x y] (path-loc-at-time ant-path i)]
        (recur (inc i) 
               (if (and (nil? (shared/thresh)) (> (shared/thresh) (image/pix-value  x y (shared/image-ref))))
                 true false)
               [x y]
               (compare-two-points local-min [x y]  :less)
               (compare-two-points local-max [x y]  :great))))))

(defn proc-all-ants [ant-init]
  (if-not (empty? ant-init)
  (map #(proc-ant %) ant-init)))


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
