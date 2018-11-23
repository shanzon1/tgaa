(ns tgaa.algo.analysis
    (:require [tgaa.struct.shared :as shared]
             [tgaa.struct.ant :as ant] 
             [tgaa.struct.image :as image])
    (:import [tgaa.hull ConvexHull Point]))


(defn is-boxed [path-1 path-2]
  (let [[start-x-1 start-y-1] (ant/ant-start-point path-1)
        [end-x-1 end-y-1] (ant/ant-end-point path-1)
        [start-x-2 start-y-2] (ant/ant-start-point path-2)
        [end-x-2 end-y-2] (ant/ant-end-point path-2)
        [leftmost-x-1 rightmost-x-1] (if (< start-x-1 end-x-1) [start-x-1 end-x-1] [end-x-1 start-x-1])
        [highest-y-1 lowest-y-1] (if (< start-y-1 end-y-1) [start-y-1 end-y-1] [end-y-1 start-y-1])
        [leftmost-x-2 rightmost-x-2] (if (< start-x-2 end-x-2) [start-x-2 end-x-2] [end-x-2 start-x-2])
        [highest-y-2 lowest-y-2] (if (< start-y-2 end-y-2) [start-y-2 end-y-2] [end-y-2 start-y-2])]
    (and (<= leftmost-x-1 rightmost-x-2)
         (>= rightmost-x-1 leftmost-x-2)
         (<= highest-y-1 lowest-y-2)
         (>= lowest-y-1 highest-y-2))))

(defn make-group [non-group]
  (loop [to-group [(first non-group)] grouping [(first non-group)] working-non-group (rest non-group)]
    (if (empty? to-group)
      [grouping  working-non-group]
      (let [filter-group (filter  #(is-boxed (first to-group) %) working-non-group)]
        (if (not (empty? filter-group))
          (recur (apply conj  (rest to-group) filter-group )
                 (apply conj grouping filter-group)
                 (filter  #(not (is-boxed (first to-group) %)) working-non-group))
          (recur (rest to-group)
                 grouping
                 working-non-group))))))

(defn make-groups [cann-path]
  (loop [non-group cann-path groups [] meta-data {} group-id 0]
    (if (empty? non-group)
      {:groups groups :group-data meta-data}
      (let [[group-res non-group-res ] (make-group non-group)]
        (recur non-group-res  
               (apply conj groups (map #(assoc % :group group-id) group-res))
               (assoc meta-data (keyword (str group-id)) (count group-res))
               (inc group-id))))))

(defn salient-regions []
  (let [groups (make-groups 
                 (shared/eval-paths))
        min-conn-group-ids (apply hash-map (flatten (filter #(>= (second %)( shared/min-conn-thresh)) (:group-data groups))))]
    (do (shared/salient-results min-conn-group-ids)
      (shared/salient-ids (:groups groups)))))
        
   
    
(defn convex-hull [paths]
  (let[points (map #(Point. (first %) (second %))(map ant/ant-end-point paths))
       cnvx (ConvexHull. (into-array tgaa.hull.Point points))
       cnvx-points (. cnvx getConvexHull)]
    (map #(vector %1 %2)
         (map #(int (. % getX)) cnvx-points)
         (map  #(int (. % getY)) cnvx-points))))


(defn edge-stats-by-group[]
  (map #(vector (image/pix-value (:local-min %) (shared/image-gry-ref)) (:group %)) (shared/salient-ids)))
