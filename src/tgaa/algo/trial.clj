(ns tgaa.algo.trial
  (:require [tgaa.struct.image :as image]
            [tgaa.struct.shared :as shared]
            [tgaa.struct.ant :as ant]))

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
    (let [mins (trial-min-local ant-paths)
          _ (shared/trial-info :mins mins)]
    (apply max mins))))

(defn trial-min-of-max[ant-paths]
  (if-not (empty? ant-paths)
    (let [maxs (trial-max-local ant-paths)
          _ (shared/trial-info :maxs maxs)]
    (apply min maxs))))

(defn trial-min-of-min[ant-paths]
  (if-not (empty? ant-paths)
    (let [mins (trial-min-local ant-paths)
          _ (shared/trial-info :mins mins)]
    (apply min mins))))

(defn trap-escaped-thresh [ant-paths]
  (if (not (empty? ant-paths))
    (let [esc-ant (escaped-ants ant-paths)
          esc_cnt (count esc-ant)
          _ (shared/trial-info-esc esc_cnt)]
      (if-not (or (nil? esc-ant) (empty? esc-ant)(< esc_cnt (shared/min-esc-paths)))
        (do (shared/trial-info :update true) 
          (trial-max-of-min esc-ant))
        (shared/thresh)))
  (shared/thresh)))

        
        