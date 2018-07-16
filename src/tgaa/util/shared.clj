(ns tgaa.util.shared)

(def trial-state (atom {:trial-num 0
                    :image-location "C:\\Users\\erudi\\OneDrive\\Activity Organizer\\Projects\\Active\\TAA Research\\Project Resources\\images\\unprocessed\\3\\1\\3_1_1.jpg"
                    :cand-paths []
                    :thresh 0}))

(defn get-config 
  "Gets Configuration set in text file" 
  [& qualified-path] 
  (if (= (first qualified-path) nil) 
    (read-string (slurp "tgaaConfig.edn"))
    (print "not implemented")))

(def config (get-config)) 

(defn get-num-trails []
  {:pre [(not (nil? (:num-trails config )))]}
  (:num-trails config))

(defn thresh-oper []
   {:pre [(not (nil? (:thresh-oper config )))]}
  (:thresh-oper config))

(defn canidates
  []
  (if-not (empty? (:cand-paths @trial-state))
  (:cand-paths @trial-state)
  (vector)))

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

(defn image-loc []
  {:pre [(not (nil? (:image-location  @trial-state)))]}
  (:image-location  @trial-state))

(defn save-image-loc [abs-path]
  (reset! trial-state (assoc @trial-state :image-location abs-path)))

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

(defn trial-num [] 
  {:pre [(not (nil? (:trial-num @trial-state)))]}
  (:trial-num @trial-state ))

(defn thresh []
  {:pre [(not (nil? (:thresh @trial-state)))]}
  (:thresh @trial-state))

(defn max-path-length []
  {:pre [(not (nil? (:max-path-length config)))]}
(:max-path-length config))

(defn num-ants []
  {:pre [(not (nil? (:num-ants config )))]}
  (:num-ants config))