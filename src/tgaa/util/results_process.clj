(ns tgaa.util.results-process
  (:require [clojure.string :as st]))

(defn get-results [] 
  (read-string 
    (slurp "results.edn")))

(defn extract-dm [res-col]
  (map #(:dice %) res-col))

(defn sample-type [sample]
  (if (nil? (clojure.string/index-of (clojure.string/lower-case (:predicted-loc sample))  "volume analysis"))
    :sample 
    :pred))
  
(defn extract-result-type [res-struc type-key]
  (filter #(= (sample-type %) type-key) res-struc))

(defn get-pred-dm []
  (tgaa.util.results-process/extract-dm 
    (tgaa.util.results-process/extract-result-type
      (tgaa.util.results-process/get-results) :pred)))

;get the prediction results
(tgaa.util.results-process/extract-dm (tgaa.util.results-process/get-results))
