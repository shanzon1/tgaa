(ns tgaa.util.reporting
   (:require [tgaa.struct.shared :as shared]))

(defn algo-metrics []
  (let [info (shared/trial-logs)]
        {:total-paths (apply + (map #(:path-count (second %)) info))
         :total-cand-paths (apply + (map #(:cand-num (second %)) info))
         :total-esc-paths (apply + (map #(:esc-num (second %)) info))
         :run-time (shared/run-time)}))