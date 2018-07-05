(ns tgaa.util.shared)

(def trail-state (atom {:trial-num 0
                    :image-location "C:\\Users\\erudi\\OneDrive\\Activity Organizer\\Projects\\Active\\TAA Research\\Project Resources\\images\\unprocessed\\3\\1\\3_1_1.jpg"
                    :cand-paths []
                    :thresh 0}))


(defn add-canidates
  ""
  [cand-paths-list]
  (if-not (empty? cand-paths-list)
  (reset! trail-state  (assoc @trail-state :cand-paths (apply conj (:cand-paths @trail-state) cand-paths-list)))))
  
  
(defn get-config 
  "Gets Configuration set in text file" 
  [& qualified-path] 
  (if (= (first qualified-path) nil) 
    (read-string (slurp "tgaaConfig.edn"))
    (print "not implemented")))

(defn inc-trial[]
  "Increments trail-state map :trial-num"
  (reset! trail-state 
          (assoc @trail-state :trial-num 
                 (inc 
                   (:trial-num @trail-state)))))


(def config (get-config))

(defn image-loc []
  (:image-location  @trail-state))

(defn save-image-loc [abs-path]
  (reset! trail-state (assoc @trail-state :image-location abs-path)))

(defn save-image-ref[image]
  (reset! trail-state (assoc @trail-state :image image)))

(defn image-ref []
  (:image @trail-state))

(defn update-thresh [thresh]
  (reset! trail-state (assoc @trail-state :thresh thresh)))

(defn trial-num []
  (:trial-num @trail-state ))

(defn thresh []
  (:thresh @trail-state))