(ns tgaa.struct.shared
  (:require [tgaa.struct.ant :as ant]))

   #"C:\\Users\\erudi\\OneDrive\\Activity Organizer\\Projects\\Active\\TAA Research\\Project Resources\\images\\unprocessed\\3\\1\\3_1_1.jpg"

(def no-threshold 1000)
   
(def trial-base {:trial-num 0
                    :image-location nil
                    :cand-paths []
                    :thresh no-threshold})

(def trial-state (atom trial-base))

(defn init-trail-state []
  (reset! trial-state trial-base))

(defn get-config 
  "Gets Configuration set in text file" 
  [& qualified-path] 
  (if (= (first qualified-path) nil) 
    (read-string (slurp "tgaaConfig.edn"))
    (print "not implemented")))

(def config (get-config)) 

(defn trial-num [] 
  {:pre [(not (nil? (:trial-num @trial-state)))]}
  (:trial-num @trial-state ))

(defn min-path-len []
  (:min-path-len config))

(defn min-esc-paths []
  (:min-esc-update config))


(defn get-num-trails []
  {:pre [(not (nil? (:num-trails config )))]}
  (:num-trails config))

(defn min-cont-thresh []
  {:pre [(not (nil? (:min-cont-thresh config )))]}
  (:min-cont-thresh config))

(defn min-conn-thresh []
  {:pre [(not (nil? (:min-conn-thresh config )))]}
  (:min-conn-thresh config))

(defn thresh-oper []
   {:pre [(not (nil? (:thresh-oper config )))]}
  (:thresh-oper config))

(defn canidates
  ([]
  (if-not (empty? (:cand-paths @trial-state))
  (:cand-paths @trial-state)
  (vector)))
  ([trial-num]
    (filter #(= (ant/ant-trial-num %) trial-num) (:cand-paths @trial-state))))

(defn hull
  ([] (:hull @trial-state))
  ([hull] 
    (reset! trial-state  (assoc @trial-state :hull hull))))

(defn trial-info 
  [key value]
  (if-not (or (nil? key) (nil? value) (not (:debug config)))
    (let [trial-key (keyword (str (trial-num)))
          info-run (if (nil? (:trial-log  @trial-state ))
                             {}
                             (:trial-log  @trial-state ))
          info-trial (if (nil? (trial-key info-run))
                       {}
                       (trial-key info-run))]
      (reset! trial-state (assoc @trial-state :trial-log  (assoc info-run trial-key (assoc info-trial key value)))))))

(defn trial-logs []
  (sort #(< (Integer. (name (first %1))) (Integer. (name (first %2)))) (:trial-log  @trial-state )))

(defn trial-info-esc
  [num]
  (trial-info :esc-num num))

(defn trial-info-cand
  [num]
  (trial-info :cand-num num))

(defn trial-info-thresh
  [num]
  (trial-info :thresh num))

(defn trial-info-path-cnt
  [num]
  (trial-info :path-count num))

(defn trial-info-error
  [error]
  (trial-info (keyword (str "error_" (gensym)))  error))

(defn trial-info-gen
  [info]
  (trial-info (keyword (str "info_" (gensym)))  info))

(defn salient-results 
  ([] (:salient-results @trial-state))
  ([salient-results]
    (if-not (empty? salient-results)
    (reset! trial-state  (assoc @trial-state :salient-results (apply conj (:salient-results @trial-state) salient-results))))))

(defn salient-ids
  ([] (:salient-ids @trial-state))
  ([salient-ids]
  (if-not (empty? salient-ids)
    (reset! trial-state  (assoc @trial-state :salient-ids (apply conj (:salient-ids @trial-state) salient-ids))))))

(defn add-canidates
  [cand-paths-list]
  (if-not (empty? cand-paths-list)
    (reset! trial-state  (assoc @trial-state :cand-paths (apply conj (:cand-paths @trial-state) cand-paths-list)))))

(defn inc-trial[]
  "Increments trial-state map :trial-num"
  (reset! trial-state 
          (assoc @trial-state :trial-num 
                 (inc 
                   (:trial-num @trial-state)))))

(defn image-loc 
  ([]
    (if (or (nil? (:image-location  config)) (not (nil? (:image-location  @trial-state))))
      (:image-location  @trial-state)
      (:image-location  config)))
  ([image-loc]
    (reset! trial-state (assoc @trial-state :image-location image-loc))))

(defn save-image-gry-ref[image]
  (reset! trial-state (assoc @trial-state :image-grayscale image)))

(defn image-gry-ref []
  {:pre [(not (nil? (:image @trial-state)))]}
  (:image-grayscale @trial-state))

(defn save-image-ref[image]
  (reset! trial-state (assoc @trial-state :image image)))

(defn image-ref []
  {:pre [(not (nil? (:image @trial-state)))]}
  (:image @trial-state))

(defn update-thresh [thresh]
  (reset! trial-state (assoc @trial-state :thresh thresh)))

(defn thresh []
  {:pre [(not (nil? (:thresh @trial-state)))]}
  (:thresh @trial-state))

(defn max-path-length []
  {:pre [(not (nil? (:max-path-length config)))]}
(:max-path-length config))

(defn num-ants []
  {:pre [(not (nil? (:num-ants config )))]}
  (:num-ants config))

(defn time-start []
   (reset! trial-state (assoc @trial-state 
                              :start-time 
                              (.getTime (java.util.Date.)))))

(defn time-end []
   (reset! trial-state (assoc @trial-state 
                              :end-time 
                              (.getTime (java.util.Date.)))))

(defn run-time []
  (- (:end-time @trial-state) (:start-time @trial-state)))
 
(defn target-intensity[val]
  (* val (case (:target-intensity config)
           "LOW" -1
           "HIGH"  1)))

(defn num-eval-ants []
  (:num-eval-ants config))

(defn eval-paths
 ([paths]
   (reset! trial-state (assoc @trial-state
                              :eval-paths
                              paths)))
 ([]
   (:eval-paths @trial-state)))