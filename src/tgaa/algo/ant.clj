(ns tgaa.algo.ant
  (:require [tgaa.util.shared :as shared]))

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
      (+ start (- (shared/max-path-length) 1))
      :else
      (+ (- start  (shared/max-path-length)) 1)))