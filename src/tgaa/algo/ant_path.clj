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
      (+ start (- (shared/max-path-length) 1))
      :else
      (+ (- start  (shared/max-path-length)) 1)))

(defn rand-ant-dir 
  "Creates safe random direction at 45 deg increments with starting point x y"
  [point] 
   (let [dir (:dir-opt (first 
               (filter #(let [lx (first (:last %))
                              ly (second (:last %))]
                          (and (>= lx 0) (>= ly 0) (< lx (image/image-width (shared/image-gry-ref))) (< ly (image/image-height (shared/image-gry-ref)))))
                       (map (fn [d] {:last [(full-path-last-point (first point)(first d)) 
                                                      (full-path-last-point (second point) (second d))] 
                                          :dir-opt d})  (shuffle ant/dir-opt)))))]
     (if (nil? dir)
       (-> (Exception. "No Direction is Possible. Please Check configuration") throw)
       dir)))

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
     

; thesh-count causing issues with path length
(defn proc-ant [ant-path]
  "Takes ants and image and generates logical paths"
  (loop [i 0 thresh-count 0 end-point nil local-min nil local-max nil]
    (if (or (>= i  (shared/max-path-length)) (>= thresh-count (shared/min-cont-thresh)))
      (let [thresh? (>= thresh-count (shared/min-cont-thresh))]
      (->> ant-path 
        (ant/ant-thresh? thresh?)
        (ant/ant-end-point (ant/path-loc-at-time ant-path (- i (shared/min-cont-thresh))))
        (ant/ant-local-min local-min)
        (ant/ant-local-max local-max)
        (ant/ant-path-length (- i (shared/min-cont-thresh)))))
      (let [[x y] (ant/path-loc-at-time ant-path i)
            _  (when (or (< x 0) (< y 0)) (println ant-path))]
        (recur (inc i) 
               (if (and (not (nil? (shared/thresh)))
                       (> (image/pix-value  x y (shared/image-gry-ref)) (shared/thresh)))
                 (inc thresh-count)
                 0)
               [x y]
               (compare-two-points local-min [x y]  :less)
               (compare-two-points local-max [x y]  :great))))))


(defn proc-all-ants [ant-init]
  (if-not (empty? ant-init)
    (let [paths (map #(proc-ant %) ant-init)]
      paths)))


