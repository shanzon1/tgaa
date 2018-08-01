(ns tgaa.algo.trial
  (:require [tgaa.util.image :as image]
            [tgaa.util.shared :as shared]
            [tgaa.algo.ant :as ant]))

(defn trial-path-point-vals[ant-paths value-func]
  "gets point value of attribute att-key  of ant paths"
  (if-not (empty? ant-paths)
    (map (fn [ant-path] 
           (let [[x y] (value-func ant-path)] (image/pix-value x y (shared/image-gry-ref)))) ant-paths)))

(defn escaped-ants [ant-paths]
  (if-not (empty? ant-paths)
    (filter #(and (not (ant/ant-thresh? %)) (< (shared/min-path-len) (ant/ant-path-length %))) ant-paths)))

(defn trapped-ants [ant-paths]
  (if-not (empty? ant-paths)
    (filter #(and (ant/ant-thresh? %) (< (shared/min-path-len) (ant/ant-path-length %)))  ant-paths)))

(defn trial-max-local[ant-paths]
  "gets max values of all paths"
  (if-not (empty? ant-paths)
    (trial-path-point-vals ant-paths ant/ant-local-max)))

(defn trial-min-of-max[ant-paths]
  (if-not (empty? ant-paths)
    (apply min (trial-max-local ant-paths))))

(defn trial-min-local[ant-paths]
  "gets min value of all paths"
  (if-not (empty? ant-paths)
    (trial-path-point-vals ant-paths ant/ant-local-min)))

(defn trial-max-of-min[ant-paths]
  (if-not (empty? ant-paths)
    (apply max (trial-min-local ant-paths))))

(defn trap-escaped-thresh [ant-paths]
  (if-not (empty? ant-paths)
    (trial-max-of-min (escaped-ants ant-paths))))

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
      (let [filter-group (filter  #(tgaa.algo.trial/is-boxed (first to-group) %) working-non-group)]
        (if (not (empty? filter-group))
          (recur (apply conj  (rest to-group) filter-group )
                 (apply conj grouping filter-group)
                 (filter  #(not (tgaa.algo.trial/is-boxed (first to-group) %)) working-non-group))
          (recur (rest to-group)
                 grouping
                 working-non-group))))))

(defn make-groups [cann-path]
  (loop [non-group cann-path groups [] meta-data {} group-id 0]
    (if (empty? non-group)
      [:groups groups :group-data meta-data]
      (let [[group-res non-group-res  ] (make-group non-group)]
        (recur non-group-res  
               (apply conj groups (map #(assoc % :group group-id) group-res))
               (assoc meta-data (keyword (str group-id)) (count group-res))
               (inc group-id))))))

; testing
;(make-groups (filter #(= (tgaa.algo.ant/ant-trial-num %) 25) (tgaa.util.shared/canidates)))

        
        