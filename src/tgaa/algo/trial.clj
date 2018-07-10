(ns tgaa.algo.trial
  (:require [tgaa.util.image :as image]
            [tgaa.util.shared :as shared]))

(defn trial-path-point-vals[ant-paths att-key]
  "gets point value of attribute att-key  of ant paths"
  (if-not (empty? ant-paths)
    (map (fn [ant-path] 
           (let [{ local-max att-key} ant-path
                 [x y] local-max] (image/pix-value x y (shared/image-ref)))) ant-paths)))

(defn escaped-ants [ant-paths]
  (if-not (empty? ant-paths)
    (filter #(= (:thresh %) false) ant-paths)))

(defn trapped-ants [ant-paths]
  (if-not (empty? ant-paths)
    (filter #(= (:thresh %) true) ant-paths)))

(defn trial-max-local[ant-paths]
  "gets max values of all paths"
  (if-not (empty? ant-paths)
    (trial-path-point-vals ant-paths :local-max)))

(defn trial-min-of-max[ant-paths]
  (if-not (empty? ant-paths)
    (apply min (trial-max-local ant-paths))))

(defn trial-min-local[ant-paths]
  "gets min value of all paths"
  (if-not (empty? ant-paths)
    (trial-path-point-vals ant-paths :local-min)))

(defn trial-max-of-min[ant-paths]
  (if-not (empty? ant-paths)
    (apply max (trial-min-local ant-paths))))

(defn trap-escaped-thresh [ant-paths]
  (if-not (empty? ant-paths)
    (trial-max-of-min (escaped-ants ant-paths))))