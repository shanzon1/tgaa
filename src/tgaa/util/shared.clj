(ns tgaa.util.shared)

(def trail-state (atom {:trial-num 0
                    :image-location "C:\\Users\\erudi\\OneDrive\\Activity Organizer\\Projects\\Active\\TAA Research\\Project Resources\\images\\unprocessed\\3\\1\\3_1_1.jpg"
                    :cand-paths []
                    :thresh 0}))

(defn get-config 
  "Gets Configuration set in text file" 
  [& qualified-path] 
  (if (= (first qualified-path) nil) 
    (read-string (slurp "tgaaConfig.edn"))
    (print "not implemented")))

(defn get-num-trails []
  {:pre [(not (nil? (:num-trails config )))]}
  (:num-trails config))

(def config (get-config))

(defn canidates
  []
  (if-not (empty? (:cand-paths @trail-state))
  (:cand-paths @trail-state)
  (vector)))

(defn add-canidates
  [cand-paths-list]
  (if-not (empty? cand-paths-list)
    (reset! trail-state  (assoc @trail-state :cand-paths (apply conj (:cand-paths @trail-state) cand-paths-list)))))

(defn inc-trial[]
  "Increments trail-state map :trial-num"
  (reset! trail-state 
          (assoc @trail-state :trial-num 
                 (inc 
                   (:trial-num @trail-state)))))

(defn image-loc []
  {:pre [(not (nil? (:image-location  @trail-state)))]}
  (:image-location  @trail-state))

(defn save-image-loc [abs-path]
  (reset! trail-state (assoc @trail-state :image-location abs-path)))

(defn save-image-ref[image]
  (reset! trail-state (assoc @trail-state :image image)))

(defn image-ref []
  {:pre [(not (nil? (:image @trail-state)))]}
  (:image @trail-state))

(defn update-thresh [thresh]
  (reset! trail-state (assoc @trail-state :thresh thresh)))

(defn trial-num [] 
  {:pre [(not (nil? (:trial-num @trail-state)))]}
  (:trial-num @trail-state ))

(defn thresh []
  {:pre [(not (nil? (:thresh @trail-state)))]}
  (:thresh @trail-state))

(defn max-path-length []
  {:pre [(not (nil? (:max-path-length config)))]}
(:max-path-length config))

(defn num-ants []
  {:pre [(not (nil? (:num-ants config )))]}
  (:num-ants config))