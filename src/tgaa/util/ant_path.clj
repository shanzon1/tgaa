(ns tgaa.util.ant-path
  (:require [tgaa.util.shared :as shared]
            [tgaa.util.image :as image]
            [tgaa.algo.trial :as trial] 
            [tgaa.algo.ant :as ant])
  (:import [java.awt.image BufferedImage]))

  
(defn rand-ant-dir 
  "Creates safe random direction at 45 deg increments with starting point x y"
  [point] 
   (:dir-opt (first 
               (filter #(let [lx (first (:last %))
                              ly (second (:last %))]
                          (and (>= lx 0) (>= ly 0) (< lx (image/image-width (shared/image-ref))) (< ly (image/image-height (shared/image-ref)))))
                       (map (fn [d] {:last [(ant/full-path-last-point (first point)(first d)) 
                                            (ant/full-path-last-point (second point) (second d))] 
                                          :dir-opt d})  (shuffle ant/dir-opt))))))

(defn random-point 
  "Get random set of coordinates"
  [num-loc]
  (partition 2
             (interleave 
               (repeatedly 
                      num-loc
                      #(rand-int 
                         (dec (image/image-width (shared/image-ref)))))
        (repeatedly 
               num-loc 
               #(rand-int 
                  (dec (image/image-height (shared/image-ref))))))))

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



(defn phero-points [num]
  "radomized ants gets one random point in each <num> paths from cand set"
  (if-not (or (empty? (shared/canidates)) (> 1 num))
    (let [num-safe (if (> num (count (shared/canidates)))
                     (count (shared/canidates))
                     num)]
      (map #(tgaa.algo.ant/path-loc-at-time %1 %2)
           (take num-safe (shuffle (shared/canidates))) 
           (repeatedly num-safe #(rand-int (shared/max-path-length)))))
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
    (if (or (>= i  (shared/max-path-length)) thresh?)
      ; needs to be extracted to shared only
      (assoc ant-path :thresh thresh? :end end-pont :local-min local-min :local-max local-max)
      (let [[x y] (ant/path-loc-at-time ant-path i)
            _  (when (or (< x 0) (< y 0)) (println ant-path))]
        (recur (inc i) 
               (if (or (nil? (shared/thresh)) (> (image/pix-value  x y (shared/image-ref)) (shared/thresh)))
                 true false)
               [x y]
               (compare-two-points local-min [x y]  :less)
               (compare-two-points local-max [x y]  :great))))))


(defn proc-all-ants [ant-init]
  (if-not (empty? ant-init)
  (map #(proc-ant %) ant-init)))


